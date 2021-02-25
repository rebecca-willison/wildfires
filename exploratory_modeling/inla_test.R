######################################################################
#
#  Script to do some exploratory analysis
#
######################################################################
setwd('/home/Becca/wildfires')

#library(reticulate)
#source_python('src/gee_weather_summary.py')
# data wrangling
library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
# for plotting
library(ggplot2)
library(viridis)
#library(leaflet)
#library(mapview)
# modeling
library(INLA)

# spatial packages
library(rgdal)
library(sf)
library(sp)
#library(spacetime)
#library(spatstat)
library(raster)
#library(maptools)

states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
california <- as_Spatial(subset(states, states$ID == 'california'))
cali_pts <- california@polygons[[1]]@Polygons[[1]]@coords

### load dataset for modeling
fires <- read_csv('data/CalFires.csv') %>% 
  data.frame %>% 
  dplyr::mutate(date = ALARM_DATE,
                month = format(date, '%m'),
                FIRE_SIZE_CLASS = cut(GIS_ACRES, 
                                      breaks = c(9.9, 100, 300, 1000, 5000, 5000000),
                                      labels = c('C', 'D', 'E', 'F', 'G'))) %>% 
  dplyr::rename(FIRE_YEAR = YEAR_)

fires_sub <- fires %>% dplyr::filter(FIRE_YEAR == 2008)
ggplot(fires_sub, aes(LONGITUDE, LATITUDE, color = month)) +
  geom_point() +
  theme_void() +
  coord_map() +
  facet_wrap(~month)

### read in a few sample layers
aspect <- raster('data/Static.aspect.tif')
slope <- raster('data/Static.slope.tif')
gHM <- raster('data/Static.gHM_median.tif')

### practice making mesh
coords <- as.matrix(fires[,c('LONGITUDE','LATITUDE')])

#mesh0 <- inla.mesh.2d(loc.domain = cali_pts,
#                      max.edge = c(0.1, 0.3), 
#                      offset = c(0.1, 0.4))

bdy <- inla.nonconvex.hull(rbind(cali_pts, coords), convex = .5)
mesh1 <- inla.mesh.2d(loc = coords, 
                      boundary = bdy, 
                      max.edge = c(0.2, 0.3), 
                      offset = c(0.1, 0.4),
                      cutoff = .1)

plot(mesh1)
points(coords, pch = 16, col = 'blue')
points(cali_pts, pch = 16, col = 'red')


### create projector matrix
A.est <- inla.spde.make.A(mesh = mesh1, loc = coords)

### create Matern object
spde <- inla.spde2.matern(mesh = mesh1)

### define the linear predictor
formula <- y ~ -1 + intercept + f(spatial.field, model = spde)

### fit model
inla_output <- inla(formula,
                    data = list(y = fires$LONGITUDE,
                                intercept = rep(1, spde$n.spde),
                                spatial.field = 1:spde$n.spde),
                    control.predictor = list(A = A.est, compute = TRUE))

### spatial prediction
s.index <- inla.spde.make.index(name = 'spatial.field',
                                n.spde = spde$n.spde)
stack.est <- inla.stack(data = list(y = fires$LONGITUDE),
                        A = list(A.est),
                        effects = list(c(s.index, list(intercept = 1))),
                        tag = 'est')
grid.x <- 50
grid.y <- 50
pred.grid <- expand.grid(x = seq(-124.4, -114.1, length.out = grid.x),
                         y = seq(32.54, 42.02, length.out = grid.y))
A.pred <- inla.spde.make.A(mesh = mesh1, loc = as.matrix(pred.grid))
stack.pred.latent <- inla.stack(data = list(xi = NA),
                                A = list(A.pred),
                                effects = list(s.index),
                                tag = 'pred.latent')
stack.pred.response <- inla.stack(data = list(y = NA),
                                  A = list(A.pred),
                                  effects = list(c(s.index, list(intercept = 1))),
                                  tag = 'pred.response')
join.stack <- inla.stack(stack.est, stack.pred.latent, stack.pred.response)

join.output <- inla(formula, 
                    data = inla.stack.data(join.stack),
                    control.predictor = list(A = inla.stack.A(join.stack),
                                             compute = TRUE))

index.pred.latent <- inla.stack.index(join.stack, tag = 'pred.latent')$data
pred.grid$z1 <- index.pred.latent

index.pred.response <- inla.stack.index(join.stack, tag = 'pred.response')$data
pred.grid$z2 <- index.pred.response

ggplot(pred.grid, aes(x, y, fill = z2)) +
  geom_tile() +
  theme_void() +
  scale_fill_viridis_c()
  
  
  



