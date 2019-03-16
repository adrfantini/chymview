#plotting module for chymview
#See chymview.R for details
#Contains:
# Routines for plotting

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

## Prepare for ggplotting

raster2df <- function(r, ..., bins=NULL, binlabels=NULL, datanames=NULL) {
#transform rasters (stacks or layers or even just filenames) in a format easily plottable with ggplot2 (simple dataframes).
#You can provide filenames and even specify varname=.
#Does not work when providing both character strings and Raster* objects at the same time
#Optionally you can provide bins and binlabels so that you can do discrete values plotting
    rasters <- unstack(stack(r, ...)) #create a list of all inputs
    if (!is.null(bins)) bins <- unique(c(-Inf, bins, Inf)) #Check bins
    if (!is.null(datanames) && length(datanames) != length(rasters)) stop("Number of datanames and number of RasterLayers provided do not match!")
    if (is.null(datanames)) datanames <- unlist(lapply(rasters, names))
    asdf <- function(rlayer) {
        out <- as.data.frame(rlayer, xy=TRUE)
        colnames(out) <- c("x", "y", "value")
        out
    }
    if (length(rasters) > 1) { #If more then 1 input
        require(dplyr)
        dfr <- bind_rows(lapply(rasters, asdf), .id="raster") #Turn rasters into dataframe
    } else { #If just one input
        dfr <- cbind(datanames, as.data.frame(r, xy=TRUE))
    }
    colnames(dfr) <- c("raster", "x", "y", "val")
    if (!is.null(bins)) { #Cut data into bins
        dfr[,5] <- cut(dfr[,4], bins, labels=binlabels)
        colnames(dfr)[5] <- "factorval"
    }
    dfr$raster <- factor(dfr$raster, labels=datanames) #assign labels to datanames
    return(dfr)
}

#create list of static ggplots
static_ggplot <- lapply(svarnames, function(x) raster2df(static[[x]]))
names(static_ggplot) <- svarnames
#Prepare a DF with cell coordinates, allows for faster plotting later in app.R
cellCoords <- static_ggplot[[1]][c("x", "y")]

scale_x_longitude <- function(min=-180, max=180, step=20, ...) {
    ewbrks <- seq(min,max,step)
    ewlbls <- unlist(lapply(ewbrks, function(x) ifelse(x < 0, paste0(x, "W"), ifelse(x > 0, paste0(x, "E"),x))))
    return(scale_x_continuous("Longitude", breaks = ewbrks, labels = ewlbls, expand = c(0, 0), limits=c(min,max), ...))
}
scale_y_latitude <- function(min=-90, max=90, step=10, ...) {
    nsbrks <- seq(min,max,step)
    nslbls <- unlist(lapply(nsbrks, function(x) ifelse(x < 0, paste0(x, "S"), ifelse(x > 0, paste0(x, "N"),x))))
    return(scale_y_continuous("Latitude", breaks = nsbrks, labels = nslbls, expand = c(0, 0), limits=c(min,max), ...))
}

iround <- function(x, interval){
    # Round numbers to desired interval
    # From https://stat.ethz.ch/pipermail/r-help/2011-December/297270.html
    interval[ifelse(x < min(interval), 1, findInterval(x, interval))]
}
#Step values and extreme values for scale_xy_latlon
steps <- iround(res(static) * c(nrow(static), ncol(static))/4, c(0.1,0.2,0.5, 1, 2, 5, 10, 20))
ext <- extent(static)

#World map
world <- map_data(mapdataname, xlim=c(ext[1], ext[2]), ylim=c(ext[3], ext[4]))
# world <- sf::st_as_sf(map(mapdataname, plot = FALSE, fill = TRUE, xlim=c(ext[1], ext[2]), ylim=c(ext[3], ext[4])))

#Functions to plot
ggvarMap <- function(oob, df, rng, addriver, bounds, pTitle) {
    if (is.null(bounds)) return()
    oob <- if (oob) {censor} else {squish}

    mytheme <- if ((bounds$xmax - bounds$xmin) > (bounds$ymax - bounds$ymin)) {
        theme(
            legend.position="bottom",
            legend.title=element_blank(),
            legend.key.width=unit(3,"cm"),
#             legend.text=element_text(angle=45, vjust=1, hjust=1),
            plot.title = element_text(hjust = 0.5)
        )
    } else {
        theme(
            legend.position="right",
            legend.title=element_blank(),
            legend.key.height=unit(3,"cm"),
            plot.title = element_text(hjust = 0.5)
        )
    }

    gg <- ggplot(df, aes(x=x, y=y, fill=val)) +
        geom_raster() + ggtitle(pTitle) +
#             geom_sf(data = world, inherit.aes=FALSE, fill=NA, colour="black") +
#             coord_sf(xlim=c(bounds[1], bounds[2]), ylim=c(bounds[3], bounds[4]), expand=c(0,0)) +
#             borders(xlim=c(bounds[1], bounds[2]), ylim=c(bounds[3], bounds[4])) +
        geom_polygon(data=world, aes(group = group, x = long, y=lat), colour=plot_mapcolor, fill=NA) +
        coord_quickmap(xlim=c(bounds$xmin, bounds$xmax), ylim=c(bounds$ymin, bounds$ymax), expand=c(0,0))+
        scale_fill_gradientn(na.value=NA, limits=c(rng[1], rng[2]), oob=oob, colours=plot_colors) +
        scale_x_continuous("Longitude") +
        scale_y_continuous("Latitude") +
        mytheme

    if (addriver) {
        gg + geom_raster(data=validRiverCoords, inherit.aes=FALSE, aes(x=lon, y=lat), fill=plot_rivercolor)
    } else {
        gg
    }
}

getTS <- function(aclick, v, maxn) {
            if (is.na(aclick$cellnr)) {
                return(rep(NA, maxn))
            } else {
                as.vector(dynData[[v]][aclick$cellnr])
            }
        }

justtext <- function(txt, size=11) {#Function to plot a tezt with ggplot
    ggplot(data.frame(x=1, y=1, label=txt), aes(x=x, y=y, label=label)) + geom_text(size=size) + theme_void()
}

ggplot_timeseries <- function(v, c1, c2) {
    if (v %in% svarnames) return(justtext("Please select a dynamic \nvariable in the 'Map' tab"))
    if (is.na(c1$cellnr) & is.na(c2$cellnr)) return(justtext("Please click and/or doubleclick\nsomewhere on the map in the 'Map' tab"))
    df <- data.frame(
        Time=rep(times, 2),
        Value=c(getTS(c1, v, ntimes), getTS(c2, v, ntimes)),
        Point=c(
            rep(paste0("i=", c1$yr, " j=", c1$xr, "; Lon=", round(c1$Lonr, 2), " Lat=", round(c1$Latr, 2)), ntimes),
            rep(paste0("i=", c2$yr, " j=", c2$xr, "; Lon=", round(c2$Lonr, 2), " Lat=", round(c2$Latr, 2)), ntimes)
        )
    )

    ggplot(na.omit(df), aes(x=Time, y=Value, color=Point)) +
        geom_line() +
        theme(
            legend.position="none",
#             legend.title=element_blank(),
            plot.title = element_text(hjust = 0.5)
        ) +
#         ggtitle("Timeseries for selected river point(s)") +
        ylab(paste0(varAtt[[v]]$long_name, " (", varAtt[[v]]$units, ")"))
}

ggplot_downstream <- function(v, t, c1, c2) {
    if (v %in% svarnames) return(justtext("Please select a dynamic \nvariable in the 'Map' tab"))
    if (is.na(c1$cellnr) & is.na(c2$cellnr)) return(justtext("Please click and/or doubleclick\nsomewhere on the map in the 'Map' tab"))
    fp1 <- flowpath(c1$cellnr)
    fp2 <- flowpath(c2$cellnr)

    lyr <- dynData[[v]][[t]] #Define data layer to use

    d1 <- lyr[fp1] #Get data
    d2 <- lyr[fp2]

    d1 <- d1[-length(d1)] #Remove last value
    d2 <- d2[-length(d2)]

    len1 <- length(d1) #Calculate lengths
    len2 <- length(d2)

    maxlen <- max(len1, len2)

    pad1 <- rep(NA, maxlen-len1) #Define paddings
    pad2 <- rep(NA, maxlen-len2)

    df <- data.frame( #Create data.frame
        Cell=rep(1:maxlen, 2),
        Value=c(pad1, d1, pad2, d2),
        Point=c(
            rep(paste0("i=", c1$yr, " j=", c1$xr, "; Lon=", round(c1$Lonr, 2), " Lat=", round(c1$Latr, 2)), maxlen),
            rep(paste0("i=", c2$yr, " j=", c2$xr, "; Lon=", round(c2$Lonr, 2), " Lat=", round(c2$Latr, 2)), maxlen)
        )
    )

    ggplot(na.omit(df), aes(x=Cell, y=Value, color=Point)) +
        geom_line() +
        theme(
            legend.position="none",
#             legend.title=element_blank(),
            plot.title = element_text(hjust = 0.5)
        ) +
#         ggtitle("Downstream series starting from selected river point(s)") +
        ylab(paste0(varAtt[[v]]$long_name, " (", varAtt[[v]]$units, ")"))
}
