
######################################################################
#
#  Script to do some exploratory analysis
#
######################################################################
library(reticulate)
source_python('data prep/src/monthly_weather_aggregate.py')
# data wrangling
library(dplyr)
library(readr)
library(tidyr)
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
fires <- read_csv('data/ca_modeling_dataset.csv') %>% 
  data.frame %>% 
  dplyr::mutate(date = as.Date(DISCOVERY_DOY, origin = paste0(FIRE_YEAR, '-01-01')),
                month = format(date, '%m'))

### get county polygons for mapping
counties <- st_as_sf(maps::map("county", plot = FALSE, fill = TRUE))
counties <- subset(counties, grepl("california", counties$ID))

### get california state boundary for point process window
states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
california <- as_Spatial(subset(states, grepl("california", states$ID)))
cal_poly <- fortify(california) %>% arrange(-order)
cal_window <- owin(poly = list(x = cal_poly$long[-1],
                               y = cal_poly$lat[-1]))

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

### do some exploration by year and by fire size class
results <- NULL
years <- unique(fires$FIRE_YEAR)
classes <- unique(fires$FIRE_SIZE_CLASS)
for(l in 1:length(layers_list)){
  for(y in 1:length(years)){
    for(c in 1:length(classes)){
      df <- fires %>% 
        dplyr::filter(FIRE_YEAR == years[y], FIRE_SIZE_CLASS == classes[c])
      df_pp <- ppp(df$LONGITUDE, df$LATITUDE, window = cal_window)
      mod.0 <- ppm(df_pp ~ 1)
      mod.1 <- ppm(df_pp ~ cov, covariates = list(cov = layers_list[[l]]))
      modsum <- summary(mod.1)
      lrt <- anova(mod.0, mod.1, test = 'LRT')
      result <- data.frame(variable = layers_names[l],
                           year = years[y],
                           size = classes[c],
                           deviance = lrt$Deviance[2],
                           lrt.pValue = lrt$`Pr(>Chi)`[2],
                           wald.pValue = 2*pnorm(-abs(modsum$coefs.SE.CI$Zval[2])))
      results <- rbind(results, result)
    }
  }
}
write.csv(results, 'results/IPPP_byYear_byClass.csv', row.names = F)

### precipitation exploratory analysis
### define analyses
year_months <- fires %>% 
  dplyr::select(FIRE_YEAR, month) %>% 
  dplyr::rename(year = FIRE_YEAR) %>% 
  dplyr::mutate(month = as.numeric(month)) %>% 
  dplyr::group_by(year, month) %>% 
  dplyr::summarise(N = n()) %>% 
  dplyr::filter(N >= 5)

month_lags <- 0:23

datasets <- merge(year_months, month_lags) %>% 
  dplyr::rename(lag = y) %>% 
  dplyr::select(-N)

### fit models
results <- NULL
for(i in 1:nrow(datasets)){
  get_monthly_precip(datasets[i, 'year'], datasets[i, 'month'] - datasets[i, 'lag'])
  layer_raster <- raster::raster('data/pr/test.pr.tif')
  layer_image <- as.im.RasterLayer(layer_raster)
  df <- fires %>% 
    dplyr::filter(FIRE_YEAR == datasets[i, 'year'],
                  month == datasets[i, 'month'])
  df_pp <- ppp(df$LONGITUDE, df$LATITUDE, window = cal_window)
  mod.0 <- ppm(df_pp ~ 1)
  mod.1 <- ppm(df_pp ~ cov, covariates = list(cov = layer_image))
  modsum <- summary(mod.1)
  lrt <- anova(mod.0, mod.1, test = 'LRT')
  result <- data.frame(lag = datasets[i, 'lag'],
                       year = datasets[i, 'year'],
                       month = datasets[i, 'month'],
                       deviance = lrt$Deviance[2],
                       lrt.pValue = lrt$`Pr(>Chi)`[2],
                       wald.pValue = 2*pnorm(-abs(modsum$coefs.SE.CI$Zval[2])))
  results <- rbind(results, result)
  file.remove('data/pr/test.pr.tif')
}
write.csv(results, 'results/IPPP_byMonthYear_Precip.csv', row.names = F)




