######################################################################
#
#  Script to do some exploratory analysis
#
######################################################################
library(reticulate)
source_python('src/gee_weather_summary.py')
# data wrangling
library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
# for plotting
library(ggplot2)
library(viridis)
library(leaflet)
library(mapview)
# spatial packages
library(sp)
library(sf)
library(spacetime)
library(spatstat)
library(raster)
library(maptools)

### load dataset for modeling
fires <- read_csv('data/CalFires.csv') %>% 
  data.frame %>% 
  dplyr::mutate(date = ALARM_DATE,
                month = format(date, '%m'),
                FIRE_SIZE_CLASS = cut(GIS_ACRES, 
                                      breaks = c(9.9, 100, 300, 1000, 5000, 5000000),
                                      labels = c('C', 'D', 'E', 'F', 'G'))) %>% 
  dplyr::rename(FIRE_YEAR = YEAR_)

### get county polygons for mapping
counties <- st_as_sf(maps::map("county", plot = FALSE, fill = TRUE))
counties <- subset(counties, grepl("california", counties$ID))

### get california state boundary for point process window
states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
california <- as_Spatial(subset(states, grepl("california", states$ID)))
cal_poly <- fortify(california) %>% arrange(-order)
cal_window <- owin(poly = list(x = cal_poly$long[-1],
                               y = cal_poly$lat[-1]))

# #####################################################################
# ### static layers (topography, population, etc)
# #####################################################################
### load the explanatory variables in raster format
### and convert to im objects for spatstat
layers_files <- list.files('data/GEE Layers')
layers_files <- layers_files[grepl('.tif',layers_files)]
layers_names <- gsub('.tif', '', layers_files)
layers_list <- lapply(layers_files, function(x){
  layer_raster <- raster(paste0('data/GEE Layers/', x))
  layer_image <- as.im.RasterLayer(layer_raster)
  return(layer_image)
})

#####################################################################
### burn index exploratory analysis
#####################################################################
### define analyses
year_months <- fires %>% 
  dplyr::select(FIRE_YEAR, month) %>% 
  dplyr::rename(year = FIRE_YEAR) %>% 
  dplyr::mutate(Month = as.numeric(month)) %>% 
  dplyr::group_by(year, Month, month) %>% 
  dplyr::summarise(N = n()) %>% 
  dplyr::filter(N >= 20) %>% 
  dplyr::mutate(date = as.Date(paste(year, Month, '01', sep = '-')))

month_lags <- 0

datasets <- merge(year_months, month_lags) %>% 
  dplyr::rename(lag = y) %>% 
  dplyr::mutate(covDate = date %m-% months(lag),
                covYear = as.numeric(format(covDate, '%Y')),
                covMonth = as.numeric(format(covDate, '%m')),
                order = row_number()) 

### fit models
results <- NULL
model_terms <- NULL
for(i in 1:nrow(datasets)){
  # get BI
  get_weather_summary_raster(datasets[i, 'covYear'], datasets[i, 'covMonth'], 1, 'month',
                             datasets[i, 'lag'] + 1, 'bi', stat = 'mean')
  bi_raster <- raster::raster('data/pr/test.bi.tif')
  bi_image <- as.im.RasterLayer(bi_raster)
  # get wind
  get_weather_summary_raster(datasets[i, 'covYear'], datasets[i, 'covMonth'], 1, 'month',
                             datasets[i, 'lag'] + 1, 'vs', stat = 'mean')
  wind_raster <- raster::raster('data/pr/test.vs.tif')
  wind_image <- as.im.RasterLayer(wind_raster)
  # filter fire dataset
  df <- fires %>% 
    dplyr::filter(FIRE_YEAR == datasets[i, 'year'],
                  month == datasets[i, 'month'])
  df_pp <- ppp(df$LONGITUDE, df$LATITUDE, window = cal_window)
  # base model
  mod.0 <- ppm(df_pp ~ 1)
  # mid model (bi only)
  mod.1 <- ppm(df_pp ~ bi, covariates = list(bi = bi_image))
  # topo interaction model
  mod.2 <- ppm(df_pp ~ bi*wind*elev*aspect*slope, 
               covariates = list(bi = bi_image,
                                 wind = wind_image,
                                 elev = layers_list[[32]],
                                 aspect = layers_list[[31]],
                                 slope = layers_list[[34]]))
  # no aspect model
  mod.3 <- ppm(df_pp ~ bi*wind*elev*slope, 
               covariates = list(bi = bi_image,
                                 wind = wind_image,
                                 elev = layers_list[[32]],
                                 slope = layers_list[[34]]))
  # save model terms from full model
  terms <- data.frame(anova(mod.2)) %>% 
    drop_na() %>% 
    arrange(desc(Deviance)) %>% 
    dplyr::mutate(dataset = i)
  terms$term <- rownames(terms)
  model_terms <- rbind(model_terms, terms)
  # do some lrt tests
  lrt01 <- anova(mod.0, mod.1, test = 'LRT')
  lrt02 <- anova(mod.0, mod.2, test = 'LRT')
  lrt03 <- anova(mod.0, mod.3, test = 'LRT')
  lrt12 <- anova(mod.1, mod.2, test = 'LRT')
  lrt13 <- anova(mod.1, mod.3, test = 'LRT')
  lrt23 <- anova(mod.2, mod.3, test = 'LRT')
  result <- data.frame(lag = datasets[i, 'lag'],
                       year = datasets[i, 'year'],
                       month = datasets[i, 'month'],
                       lrt01.pValue = round(lrt01$`Pr(>Chi)`[2], 4),
                       lrt02.pValue = round(lrt02$`Pr(>Chi)`[2], 4),
                       lrt03.pValue = round(lrt03$`Pr(>Chi)`[2], 4),
                       lrt12.pValue = round(lrt12$`Pr(>Chi)`[2], 4),
                       lrt13.pValue = round(lrt13$`Pr(>Chi)`[2], 4),
                       lrt23.pValue = round(lrt23$`Pr(>Chi)`[2], 4))
  results <- rbind(results, result)
  file.remove('data/pr/test.bi.tif')
  file.remove('data/pr/test.vs.tif')
  message(paste0(round(i/nrow(datasets), 2)*100, '% done'))
}
write.csv(results, 'results/IPPP_byMonthYear_topoWeatherInt.csv', row.names = F)
write.csv(model_terms %>%
            dplyr::left_join(datasets %>%
                               dplyr::select(year, Month, order) %>% 
                               dplyr::rename(dataset = order),
                             by = 'dataset'), 
          'results/IPPP_byMonthYear_topoWeatherInt_terms.csv', row.names = F)


