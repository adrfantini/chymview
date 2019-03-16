#Startup module for chymview
#See chymview.R for details
#Contains:
# program options definition
# options checks
# package loading

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

{### Program options definition

if(! "pacman" %in% rownames(installed.packages())) stop("CHyMview needs the 'pacman' package to manage the necessary packages. Please install 'pacman' first, by issuing install.packages('pacman') from the R command line or from Rstudio.")

suppressPackageStartupMessages(library(pacman)) #Load and check packages
suppressPackageStartupMessages(p_load(optparse)) #This is for the cool command-line stuff below. Use it!

# if(! "V8" %in% rownames(installed.packages())) {
#     rv <- system("ictp-install libv8-3.14-dev", show.output.on.console = FALSE)
#     if (rv!=0) {
#         rv2 <- system("apt install libv8-3.14-dev", show.output.on.console = FALSE)
#         if (rv!=0) {
#             stop("Was not able to install V8. I need libV8 for the colors!")
#         }
#     }
# }
if (!interactive()) {
option_list <- list(make_option(c("-s", "--static_file"),
                                type="character",
                                default=NULL,
                                help="Specify a static file to read from [default guessed from INPUT_FILE name]"),
#                     make_option(c("-r", "--rivers_shapefile"),
#                                 type="character",                                default=def_rivers,
#                                 help="Shapefile of rivers data [default %default]"),
#                     make_option(c("-b", "--basins_shapefile"),
#                                 type="character",
#                                 default=def_basins,
#                                 help="Shapefile of basins data [default %default]"),
#                     make_option(c("-x", "--explore_static"),
#                                 type="logical",
#                                 action="store_true",
#                                 help="Only explore interactive webview of the basic static fields (DEM, river network, basins). This is the default behaviour if INPUT_FILE is a static CHyM output file"),
                    make_option("--dra_thr",
                                type="double",
                                default=NULL,
                                help=paste0("Set the threshold for river recognition. Drainage area values above this threshold are considered to be river points [default auto, equivalent to about the area of", ncells_dra_thr," cells]"))
)
parser <- OptionParser(
    usage = "%prog [options] INPUT_FILE

    INPUT_FILE must be a valid CHyM NetCDF output file",
    option_list=option_list,
    epilogue=paste0("This is a tool for visualizing outputs from the CHyM model, version ", prog_version, " (", prog_date, ")\n", "For feature requests and bug reports, please write to:
    afantini@ictp.it")
)
#Gather input arguments
arguments <- parse_args(parser, positional_arguments = 1)
opt <- arguments$options
fin_d <- arguments$args[1] #Input dynamic file
}
}

{### Options checks
if( file.access(fin_d) == -1) stop(sprintf("Specified INPUT_FILE (%s) does not exist", fin_d))
}

{### Package loading
suppressPackageStartupMessages(p_load(
ncdf4,   #NetCDF file access
raster,  #Raster file manipulation
ggplot2, #Plots
scales,  #Plots extra
shiny,   #Interactive UI
leaflet, #Interactive maps
maps,    #Needed for maptools
maptools,#For borders in the ggplot maps
# sf,      #GIS magic
dplyr,   #Data manipulation
lubridate,#For manipulating dates
plotly,   #Interactive plots
# colormap, #Nice colors
htmltools#Some HTML manipulation
))
}
