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
  uid <-1 
  n <- length(slot(sp.out, "polygons"))  
  sp.out <- spChFIDs(sp.out, as.character(uid:(uid+n-1)))
  uid <- uid + n
  for (i in seq_len(length(to.combine))[-1L]){
    n <- length(slot(to.combine[[i]], "polygons"))
    to.combine[[i]] <- spChFIDs(to.combine[[i]], as.character(uid:(uid+n-1)))
    uid <- uid + n 
    sp.out<- maptools::spRbind(sp.out,to.combine[[i]]) 
  }
  return(sp.out)
}

#' create a summary map of sites and catchments
#' 
#' @param points sp object of SpatialPointsDataFrame
#' @param catchments sp object of SpatialPolygonsDataFrame
#' @param outfile the name of the image to create (must end in '.png')
#' 
#' This is a summary function for seeing the sites and the catchments as a single figure. 
#' The sites with a catchment are plotted as green, and sites w/o a catchment are red. 
#' Only catchment boundaries are plotted (no fill color used).
inventory_map <- function(points, catchments, outfile){
  catchment.ids <- as.character(unique(catchments@data$site_name))
  message(length(catchment.ids), ' sites w/ catchments (out of ', length(points),')')
  png(filename = outfile, width = 10, height = 7, res = 350, units = 'in')
  plot(points[points@data$site_name %in% catchment.ids, ], col='green', pch=20, cex=0.4)
  plot(points[!points@data$site_name %in% catchment.ids, ], col='red', pch=20, cex=0.4, add=TRUE)
  plot(catchments, add=TRUE)
  dev.off()
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
                      '<wfs:GetFeature xmlns:wfs="http://www.opengis.net/wfs" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:gml="http://www.opengis.net/gml" service="WFS" version="1.1.0" outputFormat="shape-zip" xsi:schemaLocation="http://www.opengis.net/wfs http://schemas.opengis.net/wfs/1.1.0/wfs.xsd">',
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
    mutate(site_name = make_site_name(site_no)) %>% 
    select(site_name, ogc_fid)
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


add_missing_catchments <- function(spatial.catchments, spatial.sites, spatial.points){
  shp.root <- '../1_spatial/cache/POWSTREAMS_NAD83'
  shp.files <- dir(shp.root)[grepl('.dbf', x = dir(shp.root))] %>% 
    sapply(function(x) strsplit(x, '[.]')[[1]][1], USE.NAMES = FALSE) %>% 
    mda.streams::make_site_name('nwis')
  
  no.catch <- spatial.sites$site_name[!spatial.sites$site_name %in% as.character(spatial.catchments$site_name)]
  
  new.catch <- shp.files[shp.files %in% no.catch]
  
  out <- spatial.catchments
  for (catch in new.catch){
    site.id <- mda.streams::parse_site_name(catch)
    d <- readOGR(shp.root, site.id) %>% 
      spTransform(proj4string(out))
    if (length(d) > 1){
      d <- d[2, ] # is always the second feature when this happens
      message(site.id, ' is goofy, subsetting to single polygon')
    } 
    d@data <- data.frame(site_name = catch, ogc_fid = 'NA')
    out <- rbind(out, d)
    
  }
  
  return(out)
}
