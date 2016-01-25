library(powstreams)
library(dataRetrieval)
library(mda.streams)
library(readr)
library(unitted)
library(dplyr)


min.count = 100 # per year average count
min.years = 15
skip.types = c('GW','LK','ES')


get_site_long_term <- function(huc){
  sites = filter(readNWISdata(service = "site", huc=huc, seriesCatalogOutput="true", parameterCd="00300",Access="3"), parm_cd=="00300") %>% 
    filter(data_type_cd == "dv", stat_cd == "00003") %>%
    filter(count_nu > min.count*min.years) %>% 
    mutate(years = ceiling(as.numeric(as.POSIXct(end_date)-as.POSIXct(begin_date))/365)) %>% 
    filter(years > min.years) %>% 
    filter(count_nu > min.count*years) %>% 
    filter(!(site_tp_cd %in% skip.types))
  return(sites[c('site_no','count_nu','years')])
}

all_sites <- data.frame('site_no'=c(),'count_nu'=c(),'years'=c())
hucs <- sprintf('%02.0f',1:21)
for (huc in hucs){
  all_sites = rbind(all_sites, get_site_long_term(huc))
  message(huc)
  message(paste(all_sites$site_no,collapse=','))
}



library(rgeos)
library(rgdal)
library(httr)

plot.CRS <- "+init=epsg:2163"


# states come from GDP geoserver

## -- get spatial data --
# states:
destination = tempfile(pattern = 'eco_shape', fileext='.zip')
query <- 'http://cida.usgs.gov/gdp/geoserver/wfs?service=WFS&request=GetFeature&typeName=sample:CONUS_states&outputFormat=shape-zip&version=1.0.0'
file <- GET(query, write_disk(destination, overwrite=T), progress())
shp.path <- tempdir()
unzip(destination, exdir = shp.path)
states = readOGR(shp.path, layer='CONUS_states') %>% 
  spTransform(CRS(plot.CRS))

# sites: 

site.loc <- select(readNWISsite(all_sites$site_no), dec_lat_va, dec_long_va, site_no)
site.points <- data.frame(lon=site.loc$dec_long_va, lat = site.loc$dec_lat_va)
sites.proj <- SpatialPoints(site.points, proj4string=CRS("+proj=longlat + datum=wgs84")) %>% 
  spTransform(CRS(plot.CRS))




png(filename = 'long_term_sites.png', width = 5.2, height=3.5, res=200, units = 'in')
par(mai = c(0,0,0,0), omi = c(0,0,0,0))

xlim <- c(-2034607.9,2450000.1) # specific to the transform we are using
ylim <- c(-2072574.6,727758.7)

pt.1 <- c(x=-2000000,y=-2172574.6)
pt.2 <- c(x=-2000000,y=-1972574.6)

plot(states, add = FALSE, col = 'grey90', border = 'grey50', lwd = 0.5, xlim = xlim, ylim = ylim)
plot(sites.proj, add = TRUE, col='red', bg='red', pch=23, cex=0.5)
dev.off()
write.table(all_sites, 'long_term_sites.tsv', sep='\t',row.names=F)