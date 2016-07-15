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
    browser()
    transformed.points.sp <- spTransform(x = raw.points.sp, sp::CRS(crs.string))
    if (exists('points.sp')){
      browser()
      points.sp <- maptools::spRbind(points.sp, transformed.points.sp)
    } else {
      points.sp <- transformed.points.sp
    }
    
  }
  return(points.sp)
}

create_site_catchments <- function(sites, crs.string = "+init=epsg:4326"){
  basin.ids <- mda.streams::parse_site_name(sites$site_name, out='sitenum')
  
  browser()
  raw.catchments <- hydroMap::getBasin(basin.ids)
  catchment.data <- raw.catchments@data
  updated.data <- catchment.data %>% 
    mutate(site_name = make_site_name(site_no)) %>% 
    select(site_name, ogc_fid)
  raw.catchments@data <- updated.data
  catchments <- spTransform(raw.catchments, CRS(crs.string))
  return(catchments)
}

inventory_map <- function(points, catchments, outfile){
  catchment.ids <- as.character(unique(catchments@data$site_name))
  png(filename = outfile, width = 10, height = 7, res = 350, units = 'in')
  plot(points[points@data$site_name %in% catchment.ids, ], col='green', pch=20, cex=0.4)
  plot(points[!points@data$site_name %in% catchment.ids, ], col='red', pch=20, cex=0.4, add=TRUE)
  plot(catchments, add=TRUE)
  dev.off()
}

write_shapefile <- function(obj, fileout){
  stop('asdf')#writeOGR
}