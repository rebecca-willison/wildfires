
######################################################################
#
#  Script to do some exploratory analysis
#
######################################################################

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
fires <- read_csv('data/ca_modeling_dataset.csv') %>% data.frame

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

### review results
res_plot <- results %>% 
  tidyr::separate(variable, into = c('group', 'variable_name'), 
                  sep = '\\.', remove = F) %>% 
  dplyr::mutate(feature_year = ifelse(group == 'CDLFeatures',
                                      stringr::str_remove_all(variable_name, '[a-z_]'),
                                      NA),
                pValue = round(pValue, 4)) %>% 
  dplyr::filter((group == 'CDLFeatures' & year == feature_year) | group != 'CDLFeatures')

### cropland
ggplot(res_plot %>% dplyr::filter(group == 'CDLFeatures'), 
       aes(size, pValue, fill = size)) +
  geom_boxplot() +
  theme_bw(base_size = 15) +
  guides(fill = F) +
  labs(x = 'Fire Size Class', title = 'Cropland Data Layer')

ggplot(res_plot %>% dplyr::filter(group == 'CDLFeatures'),
       aes(size, year, fill = pValue)) +
  geom_tile() +
  theme_bw(base_size = 15) +
  scale_fill_viridis_c(direction = -1)

ggplot(res_plot %>% dplyr::filter(group == 'CDLFeatures'),
       aes(year, pValue, group = size, color = size)) +
  geom_line(alpha = .5, size = 2) +
  theme_bw(base_size = 15) +
  facet_grid( . ~ size) +
  theme(axis.text.x = element_text(angle = 90))

### population density
popdens <- results %>% 
  tidyr::separate(variable, into = c('group', 'variable_name'), 
                  sep = '\\.', remove = F) %>% 
  dplyr::mutate(pValue = round(pValue, 4)) %>% 
  dplyr::filter(group == 'PopDensFeatures') %>% 
  dplyr::mutate(feature_year = gsub('gpw_v4_population_density_rev11_', '',
                                    gsub('_30_sec_population_density', '', variable_name))) %>%
  dplyr::filter(abs(year - as.numeric(feature_year)) < 3)

ggplot(popdens, aes(year, pValue, group = size, color = size)) +
  geom_line(alpha = .75, size = 2) +
  theme_bw(base_size = 15) +
  ylim(0, 1)


### static features
static <- results %>% 
  tidyr::separate(variable, into = c('group', 'variable_name'), 
                  sep = '\\.', remove = F) %>% 
  dplyr::mutate(pValue = round(pValue, 4)) %>% 
  dplyr::filter(group == 'Static') 
 
ggplot(static, aes(year, pValue, group = size, color = size)) +
  geom_hline(yintercept = .05) +
  geom_line(alpha = .8, size = 1.5) +
  theme_bw(base_size = 15) +
  ylim(0, 1) +
  facet_grid(variable_name ~ size) +
  theme(axis.text.x = element_text(angle = 90))


### landfire
landfire <- results %>% 
  tidyr::separate(variable, into = c('group', 'variable_name'), 
                  sep = '\\.', remove = F) %>% 
  dplyr::mutate(pValue = round(pValue, 4)) %>% 
  dplyr::filter(group == 'LANDFIREFeatures') %>% 
  dplyr::mutate(variable_name = gsub('CONUS_', '', variable_name))

ggplot(landfire, aes(year, pValue, group = size, color = size)) +
  geom_hline(yintercept = .05) +
  geom_line(alpha = .8, size = 1.5) +
  theme_bw(base_size = 15) +
  ylim(0, 1) +
  facet_grid(variable_name ~ size) +
  theme(axis.text.x = element_text(angle = 90))


### investigate duplicates

### exploratory plots
# counties <- st_as_sf(maps::map("county", plot = FALSE, fill = TRUE))
# counties <- subset(counties, grepl("california", counties$ID))
# 
# ggplot() +
#   geom_sf() +
#   geom_sf(data = counties, fill = 'black', color = gray(.5)) +
#   geom_point(data = fire_sub, 
#              aes(x = LONGITUDE, y = LATITUDE, color = FIRE_YEAR), 
#              alpha = .5) +
#   scale_color_viridis(option = 'plasma') +
#   theme_void(base_size = 1) +
#   guides(color = FALSE) #+
#   #facet_wrap(~FIRE_YEAR)
# 
# 
# fit <- lgcp.estK(fires.pp, covmodel=list(model='matern', nu=0.5))
# plot(fit)


