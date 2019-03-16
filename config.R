#tunables module for chymview
#See chymview.R for details
#Contains:
# Program defaults
# Definition of variable steps
# Leaflet explore map defaults
# Config checks

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

{### Program defaults
# def_rivers <- "/home/something"
# def_basins <- "something"
# seasons <- list(#List with numbers of the months for each season
#                 "winter"=c(12,1,2),
#                 "spring"=c(3,4,5),
#                 "summer"=c(6,7,8),
#                 "autumn"=c(9,10,11)
#                 )
# spinup_months <- 6 #Number of spinup months where data will be ignored
delayType_hover <- "throttle" #Plot interaction hover delay type. Possible values: c("throttle", "debounce"). Debounce is less resource-hungry.
ncells_dra_thr <- 100 #Number of cells for dra river threshold in automatic mode
mapdataname <- "world" #Name of the map layer from the maptools package. E.g. "world", "italy", "usa". Note that the mapdata package has higher resolution borders (worldHires)
varMap_height <- "700px" #Height of the static plot
plot_colors <- rev(rainbow(7)) #Which colors to use for the ggplot map
plot_rivercolor <- "black" #Default color for the river on the ggplot map
plot_mapcolor <- "white" #Default color for the borders in the ggplot map
showExtentColor<- "black" #Extent color
timevar <- "time" #Name of the time dimension
}

{### Definition of variable steps
rangestep <- c(#Step for selectRange for each variable. If NULL, 1/100 of the interquartile range will be used
    "dem"=10,
    "fdm"=1,
    "acc"=NULL,
    "lus"=1,
    "aer"=10,
    "dra"=NULL,
    "run"=NULL,
    "ctr"=1,
    "bas"=1
)
}

{### Leaflet explore map defaults
poly_tolerance <- 0.001 #Tolerance for determining river polygons
map_rivercolor <- "firebrick" #Default color for the river on the leaflet map
map_basincolor <- "forestgreen" #Default color for the basins on the leaflet map
restrictMap <- FALSE #Whether the explore map should be bounded in x-y navigation
addBasins <- FALSE #Add basins in the explore map?
tiles <- c( #Which tiles to show in the map (multiple layers accepted)
    "Hydda.Full",
    "Esri.WorldImagery"#,
#     "Esri.WorldTopoMap",
#     "Esri.DeLorme",
#     "Esri.NatGeoWorldMap",
#     "OpenStreetMap.Mapnik",
#     "OpenStreetMap.BlackAndWhite",
#     "OpenStreetMap.HOT",
#     "Thunderforest.Outdoors",
#     "Thunderforest.Landscape",
#     "CartoDB.Positron"
)
minimapTiles <- "Esri.WorldImagery" #Tiles to show in the minimap (only 1)
}

{### Config checks
    errorf <- function(mod, mess) {#Function for pretty errors
        stop(paste0("### ERROR! In ", mod, ":\n             ", mess, "!\n  ###ABORT!"))
    }

    if (! (delayType_hover %in% c("throttle", "debounce"))) errorf("config.R", "delayType_hover must be one of c('throttle', 'debounce')")

    if (interactive() & !exists("fin_d")) stop("You are running in an interactive session, but you did not specify any input file by setting the variable 'fin_d'")
}
