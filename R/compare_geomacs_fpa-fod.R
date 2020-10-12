
######################################################################
#
#  Script to extract FPA FOD wildfire data for California from 
#  sqlite database
#
######################################################################

# attach packages
library(rgdal)
library(leaflet)
library(rgeos)
source('R/src/lonlat_to_state.R')

# read in GEOMACS data
# downloaded from: https://data-nifc.opendata.arcgis.com/datasets/historic-geomac-perimeters-combined-2000-2018
# dataset name: Historic GeoMAC Perimeters Combined 2000-2018
# last updated: 3/31/2020
geomacs <- readOGR('data/Interagency_Fire_Perimeter_History_All_Years_Read_Only-shp/InteragencyFirePerimeterHistory.shp')

# identify state so can filter for California
# note that standard sp and sf methods did not work very well;
# many fires along the coast or on islands were not captured
# downloaded high resolution boundary file from gadm.org
centroids <- gCentroid(geomacs, byid = T)
USA_gadm <- st_read(dsn = 'data/gadm36_USA.gpkg', layer = 'gadm36_USA_1')
states <- lonlat_to_state(data.frame(centroids@coords), 
                          states = USA_gadm, name_col = 'NAME_1')
geomacs@data$STATE <- states
# quick map to check
unclassified_states <- subset(geomacs, is.na(STATE))
leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = unclassified_states)
# observed a few fires still not being classified as California,
# so use brute force
geomacs@data$LONGITUDE <- centroids@coords[,1]
geomacs@data$LATITUDE <- centroids@coords[,2]
geomacs@data[which(is.na(geomacs@data$STATE) & 
                           geomacs@data$LATITUDE < 42 &
                           geomacs@data$LATITUDE > 32.5 &
                           geomacs@data$LONGITUDE > -124.4 &
                           geomacs@data$LONGITUDE < -114.1), 'STATE'] <- 'California'
# filter to california fires
ca_fires <- subset(geomacs, STATE == 'California')
# check map
leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = ca_fires)

# look at subset of fires to compare to FPA FOD
fire_perims <- subset(geomacs, FIRE_YEAR == '2010' & STATE == 'California')


# filter for California fires



