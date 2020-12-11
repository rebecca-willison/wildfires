
######################################################################
#
#  Script to do some exploratory analysis
#
######################################################################
library(reticulate)
source_python('data_prep/src/gee_weather_summary.py')
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

# #####################################################################
# ### static layers (topography, population, etc)
# #####################################################################
# ### load the explanatory variables in raster format
# ### and convert to im objects for spatstat
# layers_files <- list.files('data/GEE Layers')
# layers_files <- layers_files[grepl('.tif',layers_files)]
# layers_names <- gsub('.tif', '', layers_files)
# layers_list <- lapply(layers_files, function(x){
#   layer_raster <- raster(paste0('data/GEE Layers/', x))
#   layer_image <- as.im.RasterLayer(layer_raster)
#   return(layer_image)
# })
# 
# ### do some exploration by year and by fire size class
# results <- NULL
# years <- unique(fires$FIRE_YEAR)
# classes <- unique(fires$FIRE_SIZE_CLASS)
# for(l in 1:length(layers_list)){
#   for(y in 1:length(years)){
#     for(c in 1:length(classes)){
#       df <- fires %>% 
#         dplyr::filter(FIRE_YEAR == years[y], FIRE_SIZE_CLASS == classes[c])
#       df_pp <- ppp(df$LONGITUDE, df$LATITUDE, window = cal_window)
#       mod.0 <- ppm(df_pp ~ 1)
#       mod.1 <- ppm(df_pp ~ cov, covariates = list(cov = layers_list[[l]]))
#       modsum <- summary(mod.1)
#       lrt <- anova(mod.0, mod.1, test = 'LRT')
#       result <- data.frame(variable = layers_names[l],
#                            year = years[y],
#                            size = classes[c],
#                            deviance = lrt$Deviance[2],
#                            lrt.pValue = lrt$`Pr(>Chi)`[2],
#                            wald.pValue = 2*pnorm(-abs(modsum$coefs.SE.CI$Zval[2])))
#       results <- rbind(results, result)
#     }
#   }
# }
# write.csv(results, 'results/IPPP_byYear_byClass.csv', row.names = F)
# 
# #####################################################################
# ### precipitation exploratory analysis
# #####################################################################
# ### define analyses
# year_months <- fires %>% 
#   dplyr::select(FIRE_YEAR, month) %>% 
#   dplyr::rename(year = FIRE_YEAR) %>% 
#   dplyr::mutate(Month = as.numeric(month)) %>% 
#   dplyr::group_by(year, Month, month) %>% 
#   dplyr::summarise(N = n()) %>% 
#   dplyr::filter(N >= 5) %>% 
#   dplyr::mutate(date = as.Date(paste(year, Month, '01', sep = '-')))
# 
# month_lags <- 0:23
# 
# datasets <- merge(year_months, month_lags) %>% 
#   dplyr::rename(lag = y) %>% 
#   dplyr::mutate(covDate = date %m-% months(lag),
#                 covYear = as.numeric(format(covDate, '%Y')),
#                 covMonth = as.numeric(format(covDate, '%m')))
# 
# ### fit models
# results <- NULL
# for(i in 1:nrow(datasets)){
#   get_weather_summary_raster(datasets[i, 'covYear'], datasets[i, 'covMonth'], 1,
#                              'month', 1, 'pr', stat = 'sum')
#   layer_raster <- raster::raster('data/pr/test.pr.tif')
#   layer_image <- as.im.RasterLayer(layer_raster)
#   df <- fires %>% 
#     dplyr::filter(FIRE_YEAR == datasets[i, 'year'],
#                   month == datasets[i, 'month'])
#   df_pp <- ppp(df$LONGITUDE, df$LATITUDE, window = cal_window)
#   mod.0 <- ppm(df_pp ~ 1)
#   mod.1 <- ppm(df_pp ~ cov, covariates = list(cov = layer_image))
#   modsum <- summary(mod.1)
#   lrt <- anova(mod.0, mod.1, test = 'LRT')
#   result <- data.frame(lag = datasets[i, 'lag'],
#                        year = datasets[i, 'year'],
#                        month = datasets[i, 'month'],
#                        deviance = lrt$Deviance[2],
#                        lrt.pValue = lrt$`Pr(>Chi)`[2],
#                        wald.pValue = 2*pnorm(-abs(modsum$coefs.SE.CI$Zval[2])))
#   results <- rbind(results, result)
#   file.remove('data/pr/test.pr.tif')
#   message(paste0(round(i/nrow(datasets), 2)*100, '% done'))
# }
# write.csv(results, 'results/IPPP_byMonthYear_PRCP.csv', row.names = F)
# 
# ######################################################################
# # 12-month cumulative precip
# ######################################################################
# ### define analyses
# year_months <- fires %>% 
#   dplyr::select(FIRE_YEAR, month) %>% 
#   dplyr::rename(year = FIRE_YEAR) %>% 
#   dplyr::mutate(Month = as.numeric(month)) %>% 
#   dplyr::group_by(year, Month, month) %>% 
#   dplyr::summarise(N = n()) %>% 
#   dplyr::filter(N >= 5) %>% 
#   dplyr::mutate(date = as.Date(paste(year, Month, '01', sep = '-')))
# 
# month_lags <- 12
# 
# datasets <- merge(year_months, month_lags) %>% 
#   dplyr::rename(lag = y) %>% 
#   dplyr::mutate(covDate = date %m-% months(lag),
#                 covYear = as.numeric(format(covDate, '%Y')),
#                 covMonth = as.numeric(format(covDate, '%m')))
# 
# ### fit models
# results <- NULL
# for(i in 1:nrow(datasets)){
#   get_weather_summary_raster(datasets[i, 'covYear'], datasets[i, 'covMonth'], 1, 'year',
#                      1, 'pr', stat = 'sum')
#   layer_raster <- raster::raster('data/pr/test.pr.tif')
#   layer_image <- as.im.RasterLayer(layer_raster)
#   df <- fires %>% 
#     dplyr::filter(FIRE_YEAR == datasets[i, 'year'],
#                   month == datasets[i, 'month'])
#   df_pp <- ppp(df$LONGITUDE, df$LATITUDE, window = cal_window)
#   mod.0 <- ppm(df_pp ~ 1)
#   mod.1 <- ppm(df_pp ~ cov, covariates = list(cov = layer_image))
#   modsum <- summary(mod.1)
#   lrt <- anova(mod.0, mod.1, test = 'LRT')
#   result <- data.frame(lag = datasets[i, 'lag'],
#                        year = datasets[i, 'year'],
#                        month = datasets[i, 'month'],
#                        deviance = lrt$Deviance[2],
#                        lrt.pValue = lrt$`Pr(>Chi)`[2],
#                        wald.pValue = 2*pnorm(-abs(modsum$coefs.SE.CI$Zval[2])))
#   results <- rbind(results, result)
#   file.remove('data/pr/test.pr.tif')
#   message(paste0(round(i/nrow(datasets), 2)*100, '% done'))
# }
# write.csv(results, 'results/IPPP_byMonthYear_PRCP_12m.csv', row.names = F)

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
  dplyr::filter(N >= 5) %>% 
  dplyr::mutate(date = as.Date(paste(year, Month, '01', sep = '-')))

month_lags <- 0:12

datasets <- merge(year_months, month_lags) %>% 
  dplyr::rename(lag = y) %>% 
  dplyr::mutate(covDate = date %m-% months(lag),
                covYear = as.numeric(format(covDate, '%Y')),
                covMonth = as.numeric(format(covDate, '%m')))

### fit models
results <- NULL
for(i in 2367:nrow(datasets)){
  get_weather_summary_raster(datasets[i, 'covYear'], datasets[i, 'covMonth'], 1, 'month',
                             datasets[i, 'lag'] + 1, 'bi', stat = 'mean')
  layer_raster <- raster::raster('data/pr/test.bi.tif')
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
  file.remove('data/pr/test.bi.tif')
  message(paste0(round(i/nrow(datasets), 2)*100, '% done'))
}
write.csv(results, 'results/IPPP_byMonthYear_BI.csv', row.names = F)


