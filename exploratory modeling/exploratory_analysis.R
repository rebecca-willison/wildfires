
######################################################################
#
#  Script to do some exploratory analysis
#
######################################################################

library(dplyr)
library(ggplot2)
library(leaflet)
library(mapview)
library(sp)
library(spacetime)
library(lgcp)

### load dataset for modeling
fires <- read.csv('data/ca_modeling_dataset.csv')
