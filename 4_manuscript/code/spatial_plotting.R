#' create a national map plot of sites
#' 
#' @param target_name the output filename
#' @param sites the sp object of the sites
#' 
proj.string <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"
plot_national_site_map <- function(target_name, sites){
  states <- state_map() 
  
  png(filename = target_name, width = 6, height = 4, units = 'in', res = 300)
  par(mai=c(0,0,0,0), omi=c(0,0,0,0))
  plot(states)
  sites.shifted <- spTransform(sites, proj.string) %>% 
    site_map()
  
  plot(sites.shifted, add=TRUE, col='red', pch=20, cex=0.35)
  dev.off()
}

#' adds the huc code to the data for the site
append_hucs <- function(sites){
  site.id <- mda.streams::parse_site_name(as.character(sites$site_name))
  # can chunk this into site.id vectors that are > 1, but fails with the whole thing. Too lazy to do that now:
  site.meta <- sapply(site.id, function(x){
    dataRetrieval::readNWISsite(x) %>% 
      select(huc_cd) %>% .$huc_cd
  }, USE.NAMES = FALSE)
  
  site.df <- data.frame(site_name = as.character(sites$site_name), huc = site.meta, stringsAsFactors = FALSE)
  sites@data <- site.df
  return(sites)
}

#' take map arguments and return a projected sp object
#' 
#' @param \dots arguments passed to \code{\link[maps]{map}} excluding \code{fill} and \code{plot}
#' 
to_sp <- function(...){
  library(maptools)
  library(maps)
  map <- maps::map(..., fill=TRUE, plot = FALSE)
  IDs <- sapply(strsplit(map$names, ":"), function(x) x[1])
  map.sp <- map2SpatialPolygons(map, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))
  map.sp.t <- spTransform(map.sp, CRS(proj.string))
  return(map.sp.t)
}


shifts <- list(AK = list(scale = 0.37, shift = c(90,-460), rotate = -50),
               HI = list(scale = 1, shift = c(520, -110), rotate = -35),
               PR = list(scale = 2.5, shift = c(-140, 90), rotate=20))

stuff_to_move <- list(
  AK = to_sp("world", "USA:alaska"),
  HI = to_sp("world", "USA:hawaii"),
  PR = to_sp("world", "Puerto Rico")
)


#' create the sp object for the map of shifted states
#'
state_map <- function(){
  states.out <- to_sp('state')
  for(i in names(shifts)){
    shifted <- do.call(shift_sp, c(sp = stuff_to_move[[i]], 
                                   shifts[[i]],  
                                   proj.string = proj4string(states.out),
                                   row.names = i))
    states.out <- rbind(shifted, states.out, makeUniqueIDs = TRUE)
  }
  
  return(states.out)
}

site_map <- function(sites.sp){
  huc.map <- c(AK = "19", HI = "20", PR = "21")
  sites <- sites.sp@data
  sites <- sites %>% mutate(huc = substr(huc, 1,2)) 
  sites.out <- sites.sp[!sites$huc %in% huc.map, ]
  
  for (region in names(huc.map)){
    
    sites.tmp <- sites.sp[sites$huc %in% huc.map[[region]], ]
    if (nrow(sites.tmp) > 0){
      sites.tmp <- do.call(shift_sp, c(sp = sites.tmp, ref = stuff_to_move[[region]], 
                                       shifts[[region]]))
      sites.out <- rbind(sites.out, sites.tmp)
    }
    
  }
  return(sites.out)
}


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
