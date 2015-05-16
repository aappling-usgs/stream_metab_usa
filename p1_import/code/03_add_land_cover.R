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

# geoknife script goes here