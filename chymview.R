#!/usr/bin/env Rscript

#Adriano Fantini
#afantini@ictp.it
#Tool to visualize CHyM output files

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

prog_date <- "June 2017"
prog_version <- "0.2.2"

{###CHANGELOG
#0.2.2
#In startup.R: Added "maps" to the necessary packages list
#In this file: added a small comment
}

{###TODO
##MUST
#Fix colors in static plot, add color options
#Static plot: is it faster to crop and then plot, or to plot and then crop with coordinates? Are lims faster faster then coordinates?
#Add "loading..." at boot... not easy!!!
#Add basins (works ok if you have gdal2 using gdal_polygonize.py, if not, do not bother). You can also use rasterToContour(r, n=length(unique(values(r))))
#Use gdal_polygonize even for rivers! I mean, does not work, but maybe a similar thing for lines instead of polygons exists
#Changing variable should not reset the clicked point
#Use plotly not ggplot
#Make it faster
#Check to see if the DEM is the same for both simulations, if not -> warning (popup)... maybe even cycle numbers etc.
#Option to compare different static fields, -c, that disables dynamic file viewing and enables only the explore map
# -d -> dynamic -s -> static?
#Check that DRA is always present in the static file

##SHOULD
#Transform this program into an R package on github
#Button to enable/disable closest-point clicking
#Static plot size NAMB???
#add loading logo in the dashboard: https://stackoverflow.com/questions/31440564/adding-a-company-logo-to-shinydashboard-header/32854387#32854387
# get rid of hardcoded plot_height (600px)
#Add panel with contact info/ bug reports etc?
#Add CLI option for spinup months, warning if spinup months is too high
#mchym and rchym in general info tab
#Clicks adn doubleclicks should NOT reset on var change
#Add setMaxBounds to leaflet map? -> less bandwidth -> faster?
#Substitute xyValue with xyClick
#Add more config checks
#Insert progress bars
#Click nearest-river area size should be dependant on zoom (nearPoints()) and maybe even on resolution of data. Or I could just not use nearPoints() and instead use adjacent
#Right justified rows in click table

##Could
#In Static, add panel to export plot?
#Consider using sf. The problem with coord_sf is that clicks have a different x-y coord, and plots are slower. But tick lables are nicer. NEEDS GDAL 2
#Add option to add a custom shapefile (such as map borders)
}

cat("Reading configuration file...\n")
source("config.R")

cat("Setting path...\n")
.libPaths(c("/home/netapp-clima-users1/afantini/R/x86_64-pc-linux-gnu-library/3.5", .libPaths())) #Load the library path which contains the necessary packages. You can skip this step (by commenting it) and install the packages yourself. See module "startup" to see which packages this program uses. This works only if you are within the ICTP network!!!

cat("Performing startup tasks...\n")
source("startup.R")

cat("Loading input data...\n")
source("loadinput.R")

cat("Preparing plot functions...\n")
source("plotting.R")

cat("Opening application...\n")
runApp("shiny/app.R", launch.browser=TRUE)
