
######################################################################
#
#  Script to extract LANDFIRE fuel type data for California from 
#  ArcGIS files
#  Data downloaded from USGS LANDFIRE Data Distribution Site
#  https://www.landfire.gov/viewer/
#
######################################################################

# attach packages
library(raster)
library(foreign)
library(rgdal)
library(sp)

######################################################################
### LANDFIRE DATASET
######################################################################
# set directories
landfire_dir <- 'data/lf88747118_US_200EVT'
raster_dir <- paste0(landfire_dir, '/adf')

# read in value decoder
lf_ref <- read.dbf(paste0(landfire_dir, '/', 'US_200EVT\\US_200EVT.dbf'))

# read in raster layers
lf <- raster(paste(landfire_dir, 'adf', 'w001000.adf', sep = '/'))
# get projection
new_proj <- proj4string(lf)
# convert to sp object
lf_spdf <- as(lf, "SpatialPixelsDataFrame")

######################################################################
### FIRES DATASET
######################################################################
# read in wildfire dataset
ca_fires <- read.csv('data/ca_fires.csv')
# convert to sp object
old_proj <- '+init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0'
ca_fires_sp <- SpatialPoints(coords = ca_fires[,c('LONGITUDE','LATITUDE')],
                             proj4string = CRS(old_proj))
# reproject 
ca_fires_proj <- sp::spTransform(ca_fires_sp, new_proj)

######################################################################
# JOIN AND SAVE OUTPUT
######################################################################
### get raster values for points
lf_points <- sp::over(ca_fires_sp, lf_spdf)

