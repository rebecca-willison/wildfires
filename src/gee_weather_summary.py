# import modules and initialize EE
import ee
import pandas as pd
import geopandas as gpd
import geemap
import numpy as np
ee.Initialize()

# define function to get weather summary rasters for California
def get_weather_summary_raster(year, month, day, time_unit, num_time_units, 
                               variable, stat = 'mean', filepath = 'data/pr/test.tif'):
  # check to see if variable is valid value
  gridmet_vars = ['pr','rmax','rmin','sph','srad','th','tmmn','tmmx','vs','erc',
                  'eto','bi','fm100','fm1000','etr','vpd']
  time_options = ['year','month','week','day','hour','minute','second']
  if variable in gridmet_vars:
    if time_unit in time_options:
      # define bounding geometry
      ca_geom = ee.FeatureCollection("TIGER/2018/States")\
                  .filter(ee.Filter.eq('NAME', 'California')).geometry()
      # define start and end dates
      date_start = ee.Date.fromYMD(year, month, day)
      date_end = date_start.advance(num_time_units, time_unit)
      # compute image collection
      image_coll = ee.ImageCollection('IDAHO_EPSCOR/GRIDMET')\
                .select([variable])\
                .filterDate(date_start, date_end)\
                .filterBounds(ca_geom)
      # compute image
      if stat == 'sum':
        image = image_coll.sum().clip(ca_geom)
      elif stat == 'mean':
        image = image_coll.mean().clip(ca_geom)
      else:
        print('Valid summary statistic options are mean and sum.')
        return
      # export raster
      geemap.ee_export_image(image, filepath, region = ca_geom, file_per_band = True, scale = 1000)
    else:
      print('Valid time_unit options are ' + ', '.join([str(elem) for elem in time_options]))
      return
  else:
    print('Valid variable options are ' + ', '.join([str(elem) for elem in gridmet_vars]))
    return


#features_df=pd.DataFrame([pd.DataFrame(features[_]).properties.T for _ in range(len(features))])

