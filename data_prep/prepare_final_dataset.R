######################################################################
#
#  Script to create final dataset for modeling
#
######################################################################
# data wrangling
library(dplyr)
library(tidyr)
# spatial packages
library(sf)
library(lwgeom)
# some source functions
work_dir <- '/Users/rebeccawillison/Documents/research/wildfire/wildfires'
source(paste(work_dir, 'src/lonlat_to_state.R', sep = '/'))
fig <- function(width, heigth){
  options(repr.plot.width = width, repr.plot.height = heigth)
}

### read in CalFire dataset
calfire_gdb <- 'data/fire19_1.gdb'
calfire <- st_read(dsn = paste(work_dir, calfire_gdb, sep = '/'), layer = 'firep19_1') %>%
  dplyr::filter(as.numeric(YEAR_) >= 1980,
                as.numeric(YEAR_) <= 2020,
                GIS_ACRES >= 10, 
                STATE == 'CA',
                !(is.na(ALARM_DATE))) %>%
  dplyr::mutate(newID = row_number()) %>% # make unique ID for each record
  st_cast(., 'MULTIPOLYGON') %>% # to fix Unknown WKB type 12 error
  st_make_valid(.)               # to fix invalid geometries

### join overlapping geometries on same day
dates <- st_drop_geometry(calfire) %>% 
  dplyr::group_by(ALARM_DATE) %>% 
  dplyr::summarise(N = n()) %>% 
  dplyr::filter(N > 1) %>% 
  data.frame %>% 
  dplyr::mutate(d = row_number())

dups <- NULL
new <- NULL
for(d in 1:nrow(dates)){
  message(paste('d =', d))
  x <- calfire %>% dplyr::filter(ALARM_DATE == dates[d, 'ALARM_DATE'])
  check_intersects <- try(st_intersection(x))
  if('try-error' %in% class(check_intersects)) next
  if(max(check_intersects$n.overlaps)>1){
    dup_geoms <- check_intersects %>% dplyr::filter(n.overlaps > 1)
    for(i in nrow(dup_geoms)){
      dup_rows <- st_drop_geometry(dup_geoms[i, 'origins']) %>% 
        unlist %>% unname
      dup_union_geom <- st_combine(x[dup_rows,])
      dup_union_data <- st_drop_geometry(x[dup_rows[1],])
      dup_union <- st_as_sf(dup_union_data, dup_union_geom)
      new <- rbind(new, dup_union)
    }
    dups <- rbind(dups, x[dup_rows,])
  }
}

### get centroids for original dataset
centroids <- calfire %>%
  st_centroid() %>%
  st_transform(., 4326) %>%
  st_coordinates() %>%
  data.frame %>%
  dplyr::rename(LONGITUDE = X,
                LATITUDE = Y)
calfire <- cbind(calfire, centroids)

### get centroids for new geometries
new_centroids <- new %>%
  st_centroid() %>%
  st_transform(., 4326) %>%
  st_coordinates() %>%
  data.frame %>%
  dplyr::rename(LONGITUDE = X,
                LATITUDE = Y)
new <- cbind(new, new_centroids)

### remove duplicates
calfire_new <- st_drop_geometry(calfire) %>% 
  dplyr::filter(!(newID %in% dups$newID)) %>% 
  dplyr::bind_rows(st_drop_geometry(new))

### save
write.csv(calfire_new, 'data/CalFires.csv', row.names = F)



