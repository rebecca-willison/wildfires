### convert im object to dataframe so can use ggplot
im_to_df <- function(im_obj){
  dim1 <- im_obj$dim[1]
  dim2 <- im_obj$dim[2]
  df <- mat.or.vec(dim1*dim2, 3)
  colnames(df) <- c('x','y','z')
  for(i in 1:dim1){
    for(j in 1:dim2){
      row <- i + (j-1)*dim1
      df[row, 1] <- im_obj$xcol[j]
      df[row, 2] <- im_obj$yrow[i]
      df[row, 3] <- im_obj$v[i,j]
    }
  }
  return(data.frame(df))
}

### function to make intensity maps
get_wildfire_intensity <- function(source = 'geomac', 
                                    year = '1992', 
                                    smooth = .3){
  if(source == 'calfire'){
    cal_df <- calfire %>% 
      dplyr::mutate(inCalifornia = point.in.polygon(incident_longitude, incident_latitude, 
                                                    california@polygons[[1]]@Polygons[[1]]@coords[,1],
                                                    california@polygons[[1]]@Polygons[[1]]@coords[,2])) %>%
      dplyr::filter(FIRE_YEAR == year, inCalifornia == 1) 
    cal_pp <- suppressWarnings(ppp(cal_df$incident_longitude, cal_df$incident_latitude, window = cal_window))
  }else if(source == 'fpa_fod'){
    cal_df <- fpa_fod %>%
      dplyr::mutate(inCalifornia = point.in.polygon(LONGITUDE, LATITUDE, 
                                                    california@polygons[[1]]@Polygons[[1]]@coords[,1],
                                                    california@polygons[[1]]@Polygons[[1]]@coords[,2])) %>%
      dplyr::filter(FIRE_YEAR == year, inCalifornia == 1) 
    cal_pp <- suppressWarnings(ppp(cal_df$LONGITUDE, cal_df$LATITUDE, window = cal_window))
    
  }else{
    cal_df <- geomacs_data %>%
      dplyr::mutate(inCalifornia = point.in.polygon(LONGITUDE, LATITUDE, 
                                                    california@polygons[[1]]@Polygons[[1]]@coords[,1],
                                                    california@polygons[[1]]@Polygons[[1]]@coords[,2])) %>%
      dplyr::filter(FIRE_YEAR == year, inCalifornia == 1) 
    cal_pp <- suppressWarnings(ppp(cal_df$LONGITUDE, cal_df$LATITUDE, window = cal_window))
  }
  
  dens <- density(cal_pp, sigma = smooth)
  dens_df <- im_to_df(dens) %>% 
    drop_na() %>% 
    dplyr::mutate(data_source = source,
                  year = year,
                  sigma = smooth,
                  N = cal_pp$n)
    
  return(dens_df)

}



