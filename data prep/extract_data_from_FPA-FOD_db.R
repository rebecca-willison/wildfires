
######################################################################
#
#  Script to extract FPA FOD wildfire data for California from 
#  sqlite database
#
######################################################################

# attach packages
library(DBI)
library(dplyr)
library(readr)

# open database connection 
db <- 'data/RDS-2013-0009.4_SQLITE/Data/FPA_FOD_20170508.sqlite'
con <- dbConnect(RSQLite::SQLite(), db)

# access Fires table
fires <- DBI::dbReadTable(con, 'Fires')

# close db connection
dbDisconnect(con)

# extract California fires and save as .csv
ca_fires <- fires %>% 
  dplyr::filter(STATE == 'CA')
write_csv(ca_fires, 'data/ca_fires.csv')
  
  