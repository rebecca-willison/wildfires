
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
library(dplyr)
library(readr)

######################################################################
### LANDFIRE DATASET
######################################################################
# set directories
landfire_dir <- 'data/adf'
# read in value decoder
lf_ref <- read.dbf(paste0(landfire_dir, '/', 'US_200EVT_US_200EVT.dbf'))
# LANDFIRE database
raster_name <- 'w001000.adf'
# read in raster layers
lf <- raster(paste(landfire_dir, raster_name, sep = '/'))
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
lf_points <- sp::over(ca_fires_proj, lf_spdf)
colnames(lf_points) <- 'VALUE'
# join to reference table
lf_data <- lf_points %>% 
  dplyr::left_join(ref_df, by = 'VALUE')

ca_lf_data <- cbind(ca_fires, lf_data)

write_csv(ca_lf_data, 'data/ca_fires_lf.csv')
