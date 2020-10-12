library(dplyr)
library(readr)
library(ggplot2)
library(leaflet)
library(usmap)
library(ggmap)
library(sf)
library(viridis)

fires <- suppressMessages(read_csv('data/ca_fires.csv', guess_max = 100000))

ggplot(fires, aes(FIRE_SIZE_CLASS)) +
  geom_bar(fill = 'dodgerblue2') +
  theme_bw(base_size = 20)

fire_size <- fires %>%
  dplyr::mutate(FIRE_YEAR = as.character(FIRE_YEAR)) %>%
  dplyr::group_by(FIRE_YEAR, FIRE_SIZE_CLASS) %>%
  dplyr::summarize(N = n(), .groups = 'drop')
ggplot(fire_size, aes(FIRE_YEAR, N, group = FIRE_SIZE_CLASS, color = FIRE_SIZE_CLASS)) +
  geom_line(size = 2) +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 45))

fire_cause <- fires %>%
  dplyr::mutate(FIRE_YEAR = as.character(FIRE_YEAR)) %>%
  dplyr::group_by(FIRE_YEAR, STAT_CAUSE_DESCR) %>%
  dplyr::summarize(N = n(), .groups = 'drop')
ggplot(fire_cause, aes(FIRE_YEAR, N, group = STAT_CAUSE_DESCR, color = STAT_CAUSE_DESCR)) +
  geom_line(size = 2) +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 45))

counties <- st_as_sf(map("county", plot = FALSE, fill = TRUE))
counties <- subset(counties, grepl("california", counties$ID))

ggplot() +
  geom_sf() +
  geom_sf(data = counties, fill = 'black', color = gray(.5)) +
  geom_point(data = fires, aes(x = LONGITUDE, y = LATITUDE, color = FIRE_YEAR), alpha = .05) +
  scale_color_viridis(option = 'plasma') +
  theme_void(base_size = 25)

ggplot() +
  geom_sf() +
  geom_sf(data = counties, fill = 'black', color = gray(.5)) +
  geom_point(data = fires %>% dplyr::filter(FIRE_SIZE_CLASS != 'A'), 
             aes(x = LONGITUDE, y = LATITUDE, color = FIRE_YEAR), alpha = .05) +
  scale_color_viridis(option = 'plasma') +
  theme_void(base_size = 25)

ggplot() +
  geom_sf() +
  geom_sf(data = counties, fill = 'black', color = gray(.5)) +
  geom_point(data = fires %>% dplyr::filter(!(FIRE_SIZE_CLASS %in% c('A', 'B'))), 
             aes(x = LONGITUDE, y = LATITUDE, color = FIRE_YEAR), alpha = .2) +
  scale_color_viridis(option = 'plasma') +
  theme_void(base_size = 25)