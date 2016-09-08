extract_spatial_metadata <- function(sp.path){
  
  sp <- readOGR(dirname(sp.path[1]), strsplit(basename(sp.path[1]), "\\.")[[1]][1])
  
  metadata.out <- list()
  metadata.out <- append(metadata.out, get_bbox(sp))
  metadata.out <- append(metadata.out, get_states(sp))
  return(metadata.out)
}

get_bbox <- function(sp){
  if (!grepl(pattern = 'WGS84', proj4string(sp))){
    stop('sp must be in WGS84 to calculate a valid bounding box')
  }
  bounds <- bbox(sp)
  return(list(wbbox=bounds[1,1], ebbox=bounds[1,2], 
              nbbox=bounds[2,1], sbbox=bounds[2,2]))
}

get_states <- function(sp){
  # // CONUS
  destination = tempfile(pattern = 'CONUS_States', fileext='.zip')
  query <- 'http://cida.usgs.gov/gdp/geoserver/wfs?service=WFS&request=GetFeature&typeName=derivative:CONUS_States&outputFormat=shape-zip&version=1.0.0'
  file <- GET(query, write_disk(destination, overwrite=T), progress())
  shp.path <- tempdir()
  unzip(destination, exdir = shp.path)
  states <- readOGR(shp.path, layer='CONUS_States') %>% 
    spTransform(proj4string(sp))
  overlaps <- gOverlaps(states, gSimplify(sp, tol=0.001), byid = TRUE)
  state.has.sp <- as.character(states$STATE)[colSums(overlaps) > 0]
  
  destination = tempfile(pattern = 'Alaska', fileext='.zip')
  query <- 'http://cida.usgs.gov/gdp/geoserver/wfs?service=WFS&request=GetFeature&typeName=sample:Alaska&outputFormat=shape-zip&version=1.0.0'
  file <- GET(query, write_disk(destination, overwrite=T), progress())
  shp.path <- tempdir()
  unzip(destination, exdir = shp.path)
  alaska <- readOGR(shp.path, layer='Alaska') %>% 
    spTransform(proj4string(sp)) %>% 
    gSimplify(tol=0.001)
  if (any(gOverlaps(alaska, gSimplify(sp, tol=0.001), byid = TRUE))){
    state.has.sp <- c(state.has.sp, "Alaska")
  }
  
  state.metadata <- lapply(state.has.sp, function(x) list('state-name'=x, 'state-abbr' = dataRetrieval::stateCdLookup(x)))
  return(list(states=state.metadata))
}