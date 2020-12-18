
### attach packages
library(rgdal)
library(leaflet)
library(rgeos)
library(ggplot2)
library(viridis)
library(spatstat)
library(dplyr)
library(tidyr)
library(readr)
library(ggpubr)

### source code
source('src/lonlat_to_state.R')
source('src/plot_wildfire_intensity.R')

### get california state boundary for point process window
states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
california <- as_Spatial(subset(states, grepl("california", states$ID)))
cal_poly <- fortify(california) %>% arrange(-order)
cal_window <- owin(poly = list(x = cal_poly$long[-1],
                               y = cal_poly$lat[-1]))

### load datasets
geomacs <- readOGR('data/Interagency_Fire_Perimeter_History_All_Years_Read_Only-shp/InteragencyFirePerimeterHistory.shp')
fpa_fod <- suppressMessages(read_csv('data/ca_fires.csv', guess_max = 100000)) %>% 
  dplyr::filter(!(FIRE_SIZE_CLASS %in% c('A','B')))
calfire <- suppressMessages(read_csv('data/mapdataall.csv')) %>%
  dplyr::mutate(FIRE_YEAR = format(incident_date_created, '%Y')) %>% 
  dplyr::filter(incident_acres_burned >= 10)

### prepare geomacs dataset
#1. determine centroid of fire
centroids <- gCentroid(geomacs, byid = T)
geomacs@data$LONGITUDE <- centroids@coords[,1]
geomacs@data$LATITUDE <- centroids@coords[,2]
#2. determine state from centroid (use high resolution state boundaries)
USA_gadm <- st_read(dsn = 'data/gadm36_USA.gpkg', layer = 'gadm36_USA_1')
states <- lonlat_to_state(data.frame(centroids@coords), 
                          states = USA_gadm, name_col = 'NAME_1')
geomacs@data$STATE <- states
#3. for fires with undetermined state from previous state, capture a few more
#   with loosely defined rules
geomacs@data[which(is.na(geomacs@data$STATE) & 
                     geomacs@data$LATITUDE < 42 &
                     geomacs@data$LATITUDE > 32.5 &
                     geomacs@data$LONGITUDE > -124.4 &
                     geomacs@data$LONGITUDE < -114.1), 'STATE'] <- 'California'
#4. filter for fires in California during same time period as FPA_FOD
ca_fires <- subset(geomacs, 
                   STATE == 'California' & FIRE_YEAR >= '1992' & FIRE_YEAR <= '2015')
#5. get dataframe for large fires to compare to FPA FOD
geomacs_data <- ca_fires@data %>% dplyr::filter(GIS_ACRES >= 10)

### compare intensity maps from different data sources
data_sources <- c('geomac', 'fpa_fod')
years <- 1992:2015
maps_to_make <- merge(data_sources, years)
colnames(maps_to_make) <- c('source', 'year')
calfire_maps <- data.frame(source = rep('calfire', 3),
                           year = 2013:2015)
dens_list <- apply(rbind(maps_to_make, calfire_maps), 1, function(x)
                   get_wildfire_intensity(x['source'], x['year'], smooth = .3))
wf_intensity <- do.call(rbind, dens_list)

#plot_year <- '1992'
# New facet label names for data source
fire_labs <- c('CAL FIRE', 'FPA FOD', 'GeoMAC')
names(fire_labs) <- c('calfire', 'fpa_fod', 'geomac')

pdf('results/IntensityPlots_oneScale.pdf')
for(plot_year in 1992:2015){
  p <- ggplot(wf_intensity %>% 
                dplyr::filter(year == plot_year), 
         aes(x, y)) +
    geom_tile(aes(fill = z, color = z), size = 0) +
    theme_classic(base_size = 15) +
    scale_fill_viridis(option = 'inferno',
                       name = 'Intensity') +
    scale_color_viridis(option = 'inferno') +
    coord_quickmap() +
    facet_grid(.~data_source+paste('N =', N),
               labeller = labeller(data_source = fire_labs)) +
    theme(axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          plot.title = element_text(hjust = 0.5),
          legend.position = 'bottom') +
    ggtitle(paste('Year =', plot_year)) +
    guides(color = F)
  print(p)
}
dev.off()



pdf('results/IntensityPlots_freeScale.pdf')
for(plot_year in 1992:2015){
  plot_df <- wf_intensity %>% 
    dplyr::filter(year == plot_year)
  plot_list <- NULL
  data_sources <- unique(plot_df$data_source)
  for(i in 1:length(data_sources)){
    plot_list[[i]] <- ggplot(plot_df %>% 
                             dplyr::filter(data_source == data_sources[i]), 
                             aes(x, y)) +
      geom_tile(aes(fill = z, color = z), size = 0) +
      theme_classic(base_size = 15) +
      scale_fill_viridis(option = 'inferno',
                         name = 'Intensity') +
      scale_color_viridis(option = 'inferno') +
      coord_quickmap() +
      facet_grid(.~data_source+paste('N =', N),
                 labeller = labeller(data_source = fire_labs)) +
      theme(axis.line = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank(),
            plot.title = element_text(hjust = 0.5),
            legend.position = 'bottom') +
      guides(color = F)
  }
  p <- ggarrange(plotlist = plot_list, nrow = 1)
  p <- annotate_figure(p, top = text_grob(paste('Year =', plot_year),
                                          size = 18))
  print(p)
}
dev.off()
