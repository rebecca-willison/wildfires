# import modules and initialize EE
import ee
import pandas as pd
import geopandas as gpd
import geemap
import numpy as np
ee.Initialize()

# define function to get monthly cumulative precipitation
def get_monthly_precip(year, month):
  # define bounding geometry
  ca_geom = ee.FeatureCollection("TIGER/2018/States")\
              .filter(ee.Filter.eq('NAME', 'California')).geometry()
  # define start and end dates
  date_start = ee.Date.fromYMD(year, month, 1)
  date_end = date_start.advance(1, "month")
  # compute image
  image = ee.ImageCollection('IDAHO_EPSCOR/GRIDMET')\
            .select(['pr'])\
            .filterDate(date_start, date_end)\
            .filterBounds(ca_geom)\
            .sum()\
            .mask(ca_geom)
  # export raster
  geemap.ee_export_image(image, 'data/pr/test.tif', region = ca_geom, file_per_band = True, scale = 1000)



