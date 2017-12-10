#' filter_sites sets up the rules for which sites ultimately end up in 
#' the shapefile(s)
#' 
#' @param metadata.file the path to a local version of the basic site metadata
#' @return a data.frame with site ids and lat/lon values
spatial_filter_sites <- function(metadata.file){
  metadata <- read_meta(metadata.file)
  has.coord <- !is.na(metadata$lat) & !is.na(metadata$lon)
  
  # also, filter to NWIS source only!
  is.nwis <- mda.streams::parse_site_name(metadata[, c("site_name")], out = 'database') == "nwis"
  # also, filter for sites w/ GPP!
  has.gpp <- metadata$site_name %in% mda.streams::list_sites('gpp_estBest')
  
  sites <- unitted::v(metadata[has.coord & is.nwis & has.gpp, c("site_name", "lat", "lon", "coord_datum")])
  message(nrow(sites),' sites included')
  return(sites)
}

#' create an sp::SpatialPointsDataFrame for each site in the \code{sites} data.frame
#' 
#' @param sites a data.frame with site ids and lat/lon values (from \code{spatial_filter_sites})
create_site_points <- function(sites, crs.string = "+init=epsg:4326"){
  
  crs.strings <- c('NAD83'='+init=epsg:4269', 'WGS84'='+init=epsg:4326')
  datums <- unique(sites$coord_datum)
  
  if (!all(datums %in% names(crs.strings))){
    stop('not all of ', paste(datums,collapse = ', '), ' found in ', paste(names(crs.strings),collapse = ', '))
  }
  for (datum in datums){
    use.i <- sites$coord_datum == datum
    coords <- cbind(sites$lon[use.i], sites$lat[use.i])
    raw.points.sp <- sp::SpatialPointsDataFrame(coords, 
                                                data = data.frame("site_name" = sites$site_name[use.i]), 
                                                proj4string = sp::CRS(crs.strings[[datum]]))
    transformed.points.sp <- spTransform(x = raw.points.sp, sp::CRS(crs.string))
    if (exists('points.sp')){
      points.sp <- maptools::spRbind(points.sp, transformed.points.sp)
    } else {
      points.sp <- transformed.points.sp
    }
    
  }
  return(points.sp)
}

#' combine multiple similar sp objects into one
#' 
#' @param \dots sp objects of the same class and with the same attributes
#' 
#' This function combines sp objects and returns a single sp object. 
combine_spatial <- function(...){
  to.combine <- list(...)
  sp.out <- to.combine[[1]]
  proj.string <- proj4string(sp.out)
  uid <-1 
  n <- length(slot(sp.out, "polygons"))  
  sp.out <- spChFIDs(sp.out, as.character(uid:(uid+n-1)))
  uid <- uid + n
  for (i in seq_len(length(to.combine))[-1L]){
    combine.i <- !to.combine[[i]]@data$site_name %in% sp.out@data$site_name
    if (any(combine.i)){
      n <- length(slot(to.combine[[i]][combine.i, ], "polygons"))
      bind.data <- to.combine[[i]][combine.i, ]
      bind.data <- spChFIDs(bind.data, as.character(uid:(uid+n-1)))
      uid <- uid + n 
      bind.data <- spTransform(bind.data, proj.string)
      sp.out<- maptools::spRbind(sp.out, bind.data) 
    }
  }
  return(sp.out)
}

#' get catchments from web services and return sp objects
#' 
#' @param sites data.frame of site metadata that includes field for `site_name`
#' @param feature.name what webservice endpoint to use. Currently epa_basins or gagesii_basins
#' @param ignore.sites an sp object which will be used to get site names to leave out of the request (defaults to NULL)
#' @param crs.string the coordinate reference to export as
#' 
#' returns an sp object with SpatialPolygonDataFrame for the catchments requested. 
#' Only the matches are returned, so the result is often incomplete relative to the sites requested. 
get_catchments <- function(sites, feature.name = c('epa_basins','gagesii_basins'), ignore.sites=NULL, crs.string = "+init=epsg:4326"){
  site.ids <- mda.streams::parse_site_name(sites$site_name, out='sitenum')
  if (!is.null(ignore.sites)){
    site.ids <- site.ids[!site.ids %in% mda.streams::parse_site_name(ignore.sites@data$site_name, out='sitenum')]
  }
  
  feature.name <- match.arg(feature.name)
  
  site.lookup <- c('epa_basins'='site_no','gagesii_basins'='gage_id')
  
  postURL <- "https://cida.usgs.gov/nwc/geoserver/NWC/ows"
  filterXML <- paste0('<?xml version="1.0"?>',
                      '<wfs:GetFeature xmlns:wfs="http://www.opengis.net/wfs" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" 
                      xmlns:gml="http://www.opengis.net/gml" service="WFS" version="1.1.0" outputFormat="shape-zip" 
                      xsi:schemaLocation="http://www.opengis.net/wfs http://schemas.opengis.net/wfs/1.1.0/wfs.xsd">',
                      sprintf('<wfs:Query xmlns:feature="http://owi.usgs.gov/NWC" typeName="feature:%s" srsName="EPSG:4326">', feature.name))
  
  siteText <- ""
  for(site.id in site.ids){
    siteText <- paste0(siteText,'<ogc:PropertyIsEqualTo  matchCase="true">',
                       sprintf('<ogc:PropertyName>%s</ogc:PropertyName>',site.lookup[[feature.name]]),
                       '<ogc:Literal>',site.id,'</ogc:Literal>',
                       '</ogc:PropertyIsEqualTo>')
  }
  
  filterXML <- paste0(filterXML,'<ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">',
                      '<ogc:Or>',siteText,'</ogc:Or>',
                      '</ogc:Filter>')
  
  
  filterXML <- paste0(filterXML,'</wfs:Query>',
                      '</wfs:GetFeature>')
  
  destination = tempfile(pattern = feature.name, fileext='.zip')
  
  file <- POST(postURL, body = filterXML, write_disk(destination, overwrite=T))
  filePath <- tempdir()
  unzip(destination, exdir = filePath)
  raw.catchments <- readOGR(filePath, layer=feature.name)
  catchment.data <- raw.catchments@data
  if (feature.name == 'gagesii_basins'){
    catchment.data <- rename(catchment.data, site_no=gage_id)
  }
  updated.data <- catchment.data %>% 
    mutate(site_name = make_site_name(site_no), data_source = feature.name) %>% 
    select(site_name, data_source)
  raw.catchments@data <- updated.data
  catchments <- spTransform(raw.catchments, CRS(crs.string))
  return(catchments)
}

#' write a zipped shapefile to the path specified
#' 
#' @param obj the sp object to be written to shapefile
#' @param layer the folder name and layer name for the shapefile
#' 
#' @details uses rgdal's writeOGR to write the individual shapefile files
write_shapefile <- function(obj, layer){
  shape.dir <- file.path('../1_spatial/cache',layer)
  if(!dir.exists(shape.dir)) dir.create(shape.dir, recursive=TRUE)
  writeOGR(obj, dsn=shape.dir, layer = layer, driver = 'ESRI Shapefile', overwrite_layer=TRUE)
  files <- file.path(shape.dir, dir(shape.dir))
  return(files)
}


#' create a catchment polygon formatted for NWIS
#' 
#' take a file path for a shapfile file and create an sp object that 
#' has data fields for NWIS site ID
#' 
#' @param shp.path the file path to a shapefile file relative to the \code{remake} directory
#' @return an sp object for the catchment
as.nwis_catchment <- function(shp.path, source.table){
  shp.dir <- dirname(shp.path)
  nwis.id <- strsplit(basename(shp.path), '[.]')[[1]][1]
  poly <- readOGR(dsn = shp.dir, layer = nwis.id, verbose = FALSE)
  site.name <- mda.streams::make_site_name(nwis.id, 'nwis')
  
  if (length(poly) > 1){
    poly <- poly[2, ] # is always the second feature when this happens
    message(site.name , ' is goofy, subsetting to single polygon')
  } 
  
  poly.source <- filter(source.table, site.name == tedlist) %>% .$Citation %>% as.character()
  if (length(poly.source) > 1){
    message(site.name, ' has multiple sources, using the first')
    poly.source <- poly.source[1L]
  } else if (length(poly.source) == 0 | poly.source == "Working on it"){
    poly.source = "unknown"
    message(site.name, ' source is unknown')
  }
  poly@data <- data.frame(site_name = site.name, data_source = poly.source)
  return(poly)
}
