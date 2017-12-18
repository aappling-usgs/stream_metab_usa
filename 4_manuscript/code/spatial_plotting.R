#' create a national map plot of sites
#' 
#' @param outfile the output filename
#' @param sites the sp object of the sites
#' 

#### globals part 1 ####
proj.string <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"

#### main function ####
plot_national_site_map <- function(sites, state_col='#b8bfac', sites_col='#2c5258', sites_cex=0.25){
  # prepare states & points sp objects
  states <- state_map()
  sites.shifted <- site_map(sites)
  
  # create the plot
  plot(states, col=state_col, border='white', lwd=0.5, bg=NA)
  plot(sites.shifted, add=TRUE, col=sites_col, pch=20, cex=sites_cex)
}

#### helpers ####

#' Take maps::map arguments and return a projected sp object
#' 
#' @param \dots arguments passed to \code{\link[maps]{map}} excluding \code{fill} and \code{plot}
to_sp <- function(...){
  library(maptools)
  library(maps)
  map <- maps::map(..., fill=TRUE, plot = FALSE)
  IDs <- sapply(strsplit(map$names, ":"), function(x) x[1])
  map.sp <- map2SpatialPolygons(map, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))
  map.sp.t <- spTransform(map.sp, CRS(proj.string))
  return(map.sp.t)
}

#' Create an sp object for a map of shifted states
state_map <- function(){
  states.out <- to_sp('state')
  for(i in names(shifts)){
    shifted <- do.call(
      shift_sp, 
      c(sp = stuff_to_move[[i]], 
        shifts[[i]],  
        proj.string = proj4string(states.out),
        row.names = i))
    states.out <- rbind(shifted, states.out, makeUniqueIDs = TRUE)
  }
  return(states.out)
}

#' Create an sp object for a map of shifted site points
#' 
#' @param points a SpatialPointsDataFrame
site_map <- function(points){
  points <- points %>%
    spTransform(proj.string)
    # append_hucs()
  
  huc.map <- c(AK = "19", HI = "20", PR = "21")
  sites <- points@data
  sites <- sites %>% mutate(huc2 = substr(huc, 1,2)) 
  
  # prepare site points. most can just be return, but those outside CONUS need
  # to be shifted
  points.out <- points[!sites$huc2 %in% huc.map, ]
  for (region in names(huc.map)){
    points.tmp <- points[sites$huc2 %in% huc.map[[region]], ]
    if(nrow(points.tmp) > 0){
      points.shifted <- do.call(
        shift_sp, 
        c(sp = points.tmp, ref = stuff_to_move[[region]], shifts[[region]]))
      points.out <- suppressWarnings(rbind(points.out, points.shifted))
    }
  }
  return(points.out)
}

#' Add state and HUC codes to each site point
#' 
#' @param points a SpatialPointsDataFrame
append_hucs <- function(points){
  # use dataRetrieval to get state & HUC codes
  site_nos <- mda.streams::parse_site_name(as.character(points$site_name))
  hucdat <- dataRetrieval::readNWISsite(site_nos) %>%
    mutate(site_name = mda.streams::make_site_name(site_no, 'nwis')) %>%
    left_join(dataRetrieval::stateCd, by=c('state_cd'='STATE')) %>%
    select(site_name, state=STUSAB, huc=huc_cd)
  
  # merge the new info into the SpatialPointsDataFrame
  pdat <- points@data %>%
    mutate(site_name=as.character(site_name)) %>%
    left_join(hucdat, by='site_name')
  points@data <- pdat
  
  return(points)
}

#' Shift states and points in AK, HI, and PR into a plottable position near
#' CONUS
shift_sp <- function(sp, scale = NULL, shift = NULL, rotate = 0, ref=sp, proj.string=NULL, row.names=NULL){
  if (is.null(scale) & is.null(shift) & rotate == 0){
    return(obj)
  }
  orig.cent <- rgeos::gCentroid(ref, byid=TRUE)@coords
  scale <- max(apply(bbox(ref), 1, diff)) * scale
  obj <- elide(sp, rotate=rotate, center=orig.cent, bb = bbox(ref))
  ref <- elide(ref, rotate=rotate, center=orig.cent, bb = bbox(ref))
  obj <- elide(obj, scale=scale, center=orig.cent, bb = bbox(ref))
  ref <- elide(ref, scale=scale, center=orig.cent, bb = bbox(ref))
  new.cent <- rgeos::gCentroid(ref, byid=TRUE)@coords
  obj <- elide(obj, shift=shift*10000+c(orig.cent-new.cent))
  if (is.null(proj.string)){
    proj4string(obj) <- proj4string(sp)
  } else {
    proj4string(obj) <- proj.string
  }
  
  if (!is.null(row.names)){
    row.names(obj) <- row.names
  }
  return(obj)
}

#### globals part 2 ####

shifts <- list(
  AK = list(scale = 0.37, shift = c(90,-460), rotate = -50),
  HI = list(scale = 1, shift = c(520, -110), rotate = -35),
  PR = list(scale = 2.5, shift = c(-140, 90), rotate=20))

stuff_to_move <- list(
  AK = to_sp("world", "USA:alaska"),
  HI = to_sp("world", "USA:hawaii"),
  PR = to_sp("world", "Puerto Rico")
)
