# Parse command-line arguments

args <- commandArgs(TRUE)
for (i in 1:length(args)){
  eval(parse(text=args[i])) # expecting args sb_user, sb_password, and outfile
}
cat(paste0("Setting up site data as ", sb_user, "\n"))


# Load libraries
library(dataRetrieval)
library(sbtools)
library(mda.streams)
library(powstreams)

# Enter sandbox
# sbtools::set_endpoint("production") # the default
# sbtools::set_endpoint("development") # this doesn't work off campus (except maybe with VPN?)

# Log in
if(exists("sb_password")) 
  session = authenticate_sb(sb_user, sb_password) 
else 
  session = authenticate_sb(sb_user)

# Find the sites root ("Sites_dev" folder)
sites_root <- sbtools::query_item_identifier(scheme="mda_streams_dev", type="sites_root", key="uber")

# Add sites data
site_roots <- item_list_children(sites_root$id, current_session(), limit=1000)$id
sites <- sapply(site_roots, mda.streams:::get_title, session=current_session()) #get_title should probably be exported
# alternately: sites <- query_item_identifier(scheme = "mda_streams_dev", type = "site_root", session = current_session(), limit = 1000)

# Download the data
# colorado includes HUC2s 10, 11, 13, & 14
HUC2s <- sprintf("%02.0f", c(10,11,13,14))
for(HUC2 in HUC2s) {
  download.file(sprintf("http://water.usgs.gov/GIS/dsdl/basins%s.zip", HUC2), destfile = sprintf("temp/basins%s.zip", HUC2))
  unzip(sprintf("temp/basins%s.zip", HUC2), exdir=sprintf("temp/basins%s", HUC2))
}

# Load the data into memory
library(rgdal)
shapes <- lapply(HUC2s, function(HUC2) {
  readOGR(sprintf('temp/basins%s',HUC2), layer = sprintf('basins%s',HUC2))
})

# Function to get the watershed shape, if available, from the larger shapefile
get_shed = function(nwis_id, shape){
  
  shed_names <- as.character(unique(shape$SITE_NO))
  match_id <- which(shed_names == nwis_id)
  
  
  if (length(match_id) == 0){
    return(NULL)
  } else if(length(match_id) >1){
    stop('poly match has more than one polygon')
  }
  
  shed <- shape[match_id, ]
  
  return(shed)
}


for (n in 1:length(sites)){
  site <- sites[n]
  
  # Get the data
  site_num <- strsplit(site,split = '_')[[1]][2]
  for(shape in shapes) {
    shed <- get_shed(site_num, shape)
    if(!is.null(shed)) break
  }
  
  # Check for existing item on SB
  if(!is.null(shed)){
    exists <- item_exists(scheme = 'mda_streams_dev',type = 'watershed',key = site)
  } else {
    exists = NULL
  }
  
  # Write data to file, then post to SB
  if (!is.null(shed) && !exists){
    dsn <- tempdir()
    writeOGR(shed , dsn, site, driver="ESRI Shapefile")
    cat('building ');cat(site); cat('\n')
    shp_files <- tools::list_files_with_exts(
      dir = dsn, exts = c('prj','shx','dbf','shp'), all.files = FALSE,full.names = TRUE)
    
    # as in post_watershed(site = site, files = shp_files, session = session)
    check = query_item_identifier('mda_streams_dev','watershed', site, current_session())
    if(nrow(check) == 0) {
      site_root <- query_item_identifier(scheme='mda_streams_dev', type='site_root',key=site, current_session())
      ws_id <- item_upload_create(site_root$id, files=shp_files, session=current_session())
      item_update_identifier(ws_id, scheme='mda_streams_dev', type='watershed', key=site, session=current_session())
      item_update(ws_id, list('title'=jsonlite::unbox('watershed')), current_session())
      cat('posting ');cat(shp_files); cat('\n')
    } else {
      cat('not posting ');cat(shp_files); cat('; already exists\n')
    }
    
    unlink(shp_files)
  } else {
    print(paste0('skipping ',site))
  }
}