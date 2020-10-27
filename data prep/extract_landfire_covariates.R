
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

# set directories
landfire_dir <- 'data/lf88747118_US_200EVT'
raster_dir <- paste0(landfire_dir, '/adf')

# read in value decoder
lf_ref <- read.dbf(paste0(landfire_dir, '/', 'US_200EVT\\US_200EVT.dbf'))

# read in raster layers
lf <- raster(paste(paste0(old_wd, '/', raster_dir), 'w001000.adf', sep = '/'))
lf_wgs84 <- projectRaster(lf, crs = sp::CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))




