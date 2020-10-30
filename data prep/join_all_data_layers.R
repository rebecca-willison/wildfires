
######################################################################
#
#  Join all the data
#
######################################################################

library(dplyr)
library(readr)

# wildfire data (already joined to landfire)
fires <- suppressMessages(read_csv('data/ca_fires_lf.csv', guess_max = 100000)) %>% 
  dplyr::filter(!(FIRE_SIZE_CLASS %in% c('A', 'B'))) %>% 
  dplyr::select(FOD_ID, FIRE_YEAR, DISCOVERY_DATE, DISCOVERY_DOY, STAT_CAUSE_CODE,
                STAT_CAUSE_DESCR, FIRE_SIZE, FIRE_SIZE_CLASS, LATITUDE, LONGITUDE,
                starts_with('EVT'), SAF_SRM, r, g, b, Red, Green, Blue)
              

# GEE layers
cdl <- read_csv('data/GEE Layers/CDLFeatures.csv') %>% dplyr::select(-X1)
landfire <- read_csv('data/GEE Layers/LANDFIREFeatures.csv') %>% dplyr::select(-X1)
cci <- read_csv('data/GEE Layers/MODISFeatures.csv') %>% dplyr::select(-X1)
popdens <- read_csv('data/GEE Layers/PopDensFeatures.csv') %>% dplyr::select(-X1)
static <- read_csv('data/GEE Layers/StaticFeatures.csv') %>% dplyr::select(-X1)

# join 'em 
# still need to do some feature engineering on CDL and CCI 
fires_features <- fires %>% 
  #dplyr::left_join(cdl, by = 'FOD_ID') %>% 
  dplyr::left_join(landfire, by = 'FOD_ID') %>% 
  #dplyr::left_join(cci, by = 'FOD_ID') %>% 
  dplyr::left_join(popdens, by = 'FOD_ID') %>% 
  dplyr::left_join(static, by = 'FOD_ID') 

write_csv(fires_features, 'data/ca_modeling_dataset.csv')

