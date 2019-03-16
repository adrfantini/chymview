#loadinput module for chymview
#See chymview.R for details
#Contains:
# Read dynamic filename and setup times
# Read static filename
# Sum up static and dynamic things

# This file is part of CHyMview.
#
# CHyMview is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# CHyMview is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with CHyMview.  If not, see http://www.gnu.org/licenses/.

{### Read dynamic filename and setup times
#Read metadata from dynamic file, including time definition
nc <- nc_open(fin_d)
dvarnames <- names(nc$var)
dvarnames <- dvarnames[which(!dvarnames %in% c("mchym", "rchym"))]

globAtt <- ncatt_get(nc, varid=0)
attv_d <- lapply(dvarnames, function(x) ncatt_get(nc, varid=x))
names(attv_d) <- dvarnames
times <- ncvar_get(nc, timevar)
ntimes <- length(times)
timeatt <- ncatt_get(nc, timevar)
timedef <- strsplit(timeatt$units, " ")[[1]]
timeunit <- timedef[1]
datestart <- ymd_hms(paste(timedef[3], timedef[4]), tz=timedef[5])
tz <- timedef[5]
f <- switch(timeunit,
    seconds=seconds,
    minutes=minutes,
    hours=hours,
    days=days,
    months=months,
    years=years
)
times <- datestart + f(times)
nc_close(nc)

#Read all input variables
dynData <- lapply(dvarnames, function(x) brick(fin_d, varname=x))
names(dynData) <- dvarnames

#Summary statistics for dynamic files
# sum_d <- lapply(dvarnames, function(x) summary(as.vector(values(dynData[[x]]))))
# names(sum_d) <- dvarnames
# sum_d <- bind_rows(sum_d)
#Only for first layer...
sum_d <- lapply(dvarnames, function(x) summary(dynData[[x]][[1]]))
sum_d <- as.data.frame(sum_d)
colnames(sum_d) <- dvarnames

#Automatic calculation of dra threshold for river recognition
if (!interactive()) {
    if (is.null(arguments$options$dra_thr)) {
        dra_thr <- (globAtt$approximate_resolution_in_meters/1000)**2*ncells_dra_thr
    } else {
        dra_thr <- arguments$options$dra_thr
    }
} else {
    dra_thr <- (globAtt$approximate_resolution_in_meters/1000)**2*ncells_dra_thr
}
}

{### Read static filename
#Generate static filename
if (!interactive()) {
    if (is.null(arguments$options$static_file)) {
        basen <- basename(fin_d)
        dirn <- dirname(fin_d)
        fin_s <- unlist(strsplit(basen, "_"))
        lenn <- length(fin_s)
        basen <- paste0(paste0(fin_s[1:lenn-1], collapse="_"), ".static_fields.nc")
        fin_s <- paste0(dirn, "/", basen)
    } else {
        fin_s <- arguments$options$static_file
    }
} else {
    basen <- basename(fin_d)
    dirn <- dirname(fin_d)
    fin_s <- unlist(strsplit(basen, "_"))
    lenn <- length(fin_s)
    basen <- paste0(paste0(fin_s[1:lenn-1], collapse="_"), ".static_fields.nc")
    fin_s <- paste0(dirn, "/", basen)
}
#Read metadata from static filename
nc <- nc_open(fin_s)
svarnames <- names(nc$var)
svarnames <- svarnames[which(!svarnames %in% c("mchym", "rchym"))]
mchym <- ncvar_get(nc, "mchym")
rchym <- ncvar_get(nc, "rchym")
attv_s <- lapply(svarnames, function(x) ncatt_get(nc, varid=x))
names(attv_s) <- svarnames
nc_close(nc)


#Load all variables of the static filename in a stack
static <- stack(lapply(svarnames, function(x) raster(fin_s, varname=x)))
names(static) <- svarnames
static <- readAll(static) #Read all the data into memory

if (!compareRaster(static[[1]], dynData[[1]], stopiffalse=FALSE)) stop("Static and dynamic dataset do not share the same grid!")

#Mask out the sea
seamask <- static$lus==15
static <- mask(static, seamask, maskvalue=1)

#Get river data
river <- static[["dra"]]
river[river<dra_thr] <- NA
riverMask <- river/river
validRiverCells <- which(!is.na(values(river)))
validRiverRowCol <- rowColFromCell(river, validRiverCells)
validRiverCoords <- data.frame(coordinates(river)[validRiverCells,], celln=validRiverCells, validRiverRowCol)
validRiverCoords$fdm <- static[["fdm"]][validRiverCells]
colnames(validRiverCoords) <- c("lon", "lat", "celln", "row", "col", "fdm")
validRiverDra <- river[validRiverCells]
validRiverLabels <- sprintf(
  "i=%d j=%d<br/>Lat=%g Lon=%g<br/><strong>Drainage area:</strong><br/>%g km<sup>2</sup>",
  validRiverRowCol[,2], validRiverRowCol[,1], validRiverCoords[,2], validRiverCoords[,1], validRiverDra
) %>% lapply(htmltools::HTML)

#Transform river into polygons
validRiverPoints <- SpatialPointsDataFrame(
    validRiverCoords[,1:2],
    data = data.frame(data = validRiverDra),
    proj4string = CRS("+init=epsg:4326")
    )
riverpolys <- as(SpatialPixelsDataFrame(validRiverPoints, validRiverPoints@data, tolerance = poly_tolerance),"SpatialPolygonsDataFrame")
#Transform into Leaflet-ready projection ##LEAFLET DOES NOT LIKE IT! it wants lat-lon data... what a waste of CPU cycles!
# riverpolys <- spTransform(riverpolys, "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs")

#Calculate the basins and transform into polygons
if (addBasins) {
basin <- boundaries(static[["bas"]], classes=TRUE, asNA=TRUE)
validBasinCells <- which(!is.na(values(basin)))
validBasinCoords <- as.data.frame(coordinates(basin)[validBasinCells,])
colnames(validBasinCoords) <- c("lon", "lat")
validBasinPoints <- SpatialPointsDataFrame(
    validBasinCoords,
    data = data.frame(data = rep(1, length(validBasinCells))),
    proj4string = CRS("+init=epsg:4326")
    )
basinpolys <- as(SpatialPixelsDataFrame(validBasinPoints, validBasinPoints@data, tolerance = poly_tolerance),"SpatialPolygonsDataFrame")
# # # # #Even better turn them into shp:
# # # # basinshp <- rasterToPolygons(bas, dissolve=TRUE)
# # # # #Much faster, but, on ubuntu, requires python-gdal and gdal-bin
# # # # source("gdal_polygonizeR.R")
# # # # basinshp <- gdal_polygonizeR(static[["bas"]])
}

#Summary statistics for static files
sum_s <- as.data.frame(summary(static))

#Define a function to calculate the flowpath of a cell, needed for downstream plots
ncols <- ncol(river)
flowpath <- function(cell) {
    if (is.na(cell)) return(NA)
    flowp <- NULL
    nextcell <- cell
    while(TRUE){
        nextcell <- switch(validRiverCoords[which(validRiverCells==nextcell), "fdm"],
            nextcell-ncols-1,#NW, 1
            nextcell-ncols,  #N, 2
            nextcell-ncols+1,#NE, 3
            nextcell+1,   #E, 4
            nextcell+ncols+1,#SE, 5
            nextcell+ncols,  #S, 6
            nextcell+ncols-1,#SW, 7
            nextcell-1    #W, 8
            #Other values are NULL
        )
        if (is.null(nextcell)) {
            break
        }
        if (!(nextcell %in% (validRiverCoords$celln))) {
            break
        }
        flowp <- c(flowp, nextcell)
    }
    return(flowp)
}
}

{### Sum up static and dynamic things
varAtt <- c(attv_s, attv_d)
varSum <- cbind(sum_s, sum_d)
}
