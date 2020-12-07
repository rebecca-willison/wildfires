
######################################################################
#
#  Download NOAA climate indices from
#  https://www.cpc.ncep.noaa.gov/data/teledoc/telecontents.shtml
#
######################################################################

library(RCurl)

url <- 'ftp://ftp.cpc.ncep.noaa.gov/cwlinks/'

files <- c('norm.daily.aao.index.b790101.current.ascii',
           'norm.daily.ao.index.b500101.current.ascii',
           'norm.daily.nao.index.b500101.current.ascii',
           'norm.daily.pna.index.b500101.current.ascii')

indices <- NULL
for (filename in files){
  fn <- paste0(getwd(), '/data/noaa_indices/', filename)
  download.file(paste0(url, filename), fn)
  tmp <- read.fwf(fn, widths = c(4, 3, 3, 7), row.names = NULL, header = F)
  colnames(tmp) <- c('year', 'month', 'day', 'value')
  tmp$index <- gsub('\\.','',substr(filename, 12, 14))
  indices <- rbind(indices, tmp)
}

write.csv(indices, 'data/noaa_indices/NOAA_climate_indices.csv', row.names = F)




