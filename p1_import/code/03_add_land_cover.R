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
library(magrittr)

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

# Add geoknife
for (n in 1:length(sites)){
  site <- sites[n]
  
  # Get the data. This section breaks, probably because watershed_wfs is a private url. How to get around that?
  watershed_wfs <- 
    query_item_identifier(scheme = 'mda_streams_dev', type = 'watershed', key = site) %$% # cool magrittr pipe: exposes names to next fun
    item_get(id) %>%
    mda.streams:::match_url_distro("ScienceBase WFS Service")
  stencil <- webgeom(url=watershed_wfs)
  fabric <- webdata(url="http://cida.usgs.gov/thredds/dodsC/mows/sr")
  job <- geoknife(stencil, fabric)
  check(job) #ultimately get ProcessFailed
  watershed_data <- loadOutput(job)
  
  # The example is nice and works fine
  #   stencil <- webgeom("state::CO")
  #   fabric <- webdata(list(
  #     times = as.POSIXct(c('1895-01-01','1899-01-01')),
  #     url = 'http://cida.usgs.gov/thredds/dodsC/prism',
  #     variables = 'ppt'))
  #   job <- geoknife(stencil, fabric) #ignore the checkAttrNamespaces warning for now - it's a known issue
  #   check(job)$status # get Process successful
  #   data <- loadOutput(job)
  #   plot(data[,1:2], ylab = variables(fabric))

  # This example doesn't work
  #   stencil <- webgeom("state::CO")
  #   fabric <- webdata(url="http://cida.usgs.gov/thredds/dodsC/mows/sr")
  #   job <- geoknife(stencil, fabric) #ignore the checkAttrNamespaces warning for now - it's a known issue
  #   check(job) # breaks. won't show or terminate job.
  
  # Check for existing item on SB
  if(!is.null(shed)){
    exists <- item_exists(scheme = 'mda_streams_dev',type = 'geostat', key = site)
  } else {
    exists = NULL
  }
  
  # Write data to file, then post to SB. Switch to more descriptive name that
  # geostat once we know what can be acquired
  if (watershed_data_getting_worked && !exists){

    # write to file here
    
    # as in post_watershed(site = site, files = shp_files, session = session)
    #     check = query_item_identifier('mda_streams_dev','geostat', site, current_session())
    #     if(nrow(check) == 0) {
    #       site_root <- query_item_identifier(scheme='mda_streams_dev', type='site_root',key=site, current_session())
    #       ws_id <- item_upload_create(site_root$id, files=shp_files, session=current_session())
    #       item_update_identifier(ws_id, scheme='mda_streams_dev', type='geostat', key=site, session=current_session())
    #       item_update(ws_id, list('title'=jsonlite::unbox('geostat')), current_session())
    #       cat('posting ');cat(shp_files); cat('\n')
    #     } else {
    #       cat('not posting ');cat(shp_files); cat('; already exists\n')
    #     }
    
    # clean up locally written files
    #    unlink(shp_files)
  } else {
    print(paste0('skipping ',site))
  }
}