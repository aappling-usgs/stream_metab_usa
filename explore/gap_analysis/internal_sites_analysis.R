library(powstreams)
library(dataRetrieval)
library(mda.streams)
library(readr)
library(unitted)
library(dplyr)


min.count = 100
skip.types = c('GW','LK','ES')

get_sites = function(huc){
  sites = filter(readNWISdata(service = "site", huc=huc, seriesCatalogOutput="true",outputDataTypeCd="iv",parameterCd="00300",Access="3"), parm_cd=="00300") %>% 
    filter(count_nu > min.count) %>% 
    filter(!(site_tp_cd %in% skip.types))
  return(sites[c('site_no','count_nu')])
}

get_site_long_term <- function(huc){
  sites = filter(readNWISdata(service = "site", huc=huc, seriesCatalogOutput="true", parameterCd="00300",Access="3"), parm_cd=="00300") %>% 
    filter(data_type_cd == "dv") %>%
    filter(count_nu > 1500) %>% 
    filter(!(site_tp_cd %in% skip.types))
  return(sites[c('site_no','count_nu')])
}

all_sites <- data.frame('site_no'=c(),'count_nu'=c())
hucs <- sprintf('%02.0f',1:21)
for (huc in hucs){
  all_sites = rbind(all_sites, get_sites(huc))
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

metabDataAll <- get_meta()
metab.sites <- metabDataAll[which(metabDataAll$manual.assessment %in% c("accept", "examine")), ]$site_name
missing.sites <- !(all_sites$site_no %in% parse_site_name(metabDataAll$site_name))
total.days <- all_sites$count_nu[missing.sites]
message('total internal stream days: ', sum(total.days))

site.loc <- get_site_coords(metab.sites, format="normal")
site.points <- data.frame(lon=site.loc$lon, lat = site.loc$lat)
sp2 <- SpatialPoints(site.points, proj4string=CRS("+proj=longlat + datum=wgs84")) %>% 
  spTransform(CRS(plot.CRS))

site.loc <- select(readNWISsite(all_sites$site_no[missing.sites]), dec_lat_va, dec_long_va, site_no)
site.points <- data.frame(lon=site.loc$dec_long_va, lat = site.loc$dec_lat_va)
internal.sites <- SpatialPoints(site.points, proj4string=CRS("+proj=longlat + datum=wgs84")) %>% 
  spTransform(CRS(plot.CRS))




png(filename = 'internal_00300_sites.png', width = 5.2, height=3.5, res=200, units = 'in')
par(mai = c(0,0,0,0), omi = c(0,0,0,0))

xlim <- c(-2034607.9,2450000.1) # specific to the transform we are using
ylim <- c(-2072574.6,727758.7)

pt.1 <- c(x=-2000000,y=-2172574.6)
pt.2 <- c(x=-2000000,y=-1972574.6)

plot(states, add = FALSE, col = 'grey90', border = 'grey50', lwd = 0.5, xlim = xlim, ylim = ylim)
plot(internal.sites, add = TRUE, col='red', bg='red', pch=23, cex=0.5)
plot(sp2, add = TRUE, col='dodgerblue', pch=20, cex=0.5)
points(pt.1[['x']],pt.1[['y']],col='dodgerblue', pch=20, cex=1)
points(pt.2[['x']],pt.2[['y']],col='red', bg='red', pch=23, cex=1)
text(pt.1[['x']],pt.1[['y']],'Powstreams sites (Accept/Examine)',pos=4)
text(pt.2[['x']],pt.2[['y']],'Internal only sites',pos=4)
dev.off()
