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

# find the project root ("Continental Stream Metabolism" folder)
true_sites_root <- sbtools::query_item_identifier(scheme="mda_streams", type="project_root", key="uber")$id
project_root <- sbtools::item_get_parent(true_sites_root)

# create a sandbox sites folder to work with from here on
# only do once (need to find way to try these lines without damaging things if the )
sites_root <- sbtools::query_item_identifier(scheme="mda_streams_dev", type="sites_root", key="uber", session=current_session())
if(nrow(sites_root) == 0) {
  sites_root <- sbtools::item_create(parent_id = project_root, title="Sites_dev")
  sbtools::item_update_identifier(id=sites_root, scheme="mda_streams_dev", type="sites_root", key="uber") # true sites root currently has type="project_root". i find that confusing.
}

# get list of sites from NWIS & put list on SB
sites_list <- mda.streams::init_nwis_sites(p_codes=c('00010', '00060', '00300'), stateCd="CO")
create_sites_out <- lapply(sites_list, function(site) {
  # like mda.streams::create_site(site)
  site_root <- query_item_identifier(scheme="mda_streams_dev", type="site_root", key=site, session=current_session())
  if(nrow(site_root) == 0) {
    site_root_id <- item_create(parent_id=sites_root$id, title=site)
    item_update_identifier(site_root_id, scheme="mda_streams_dev", type="site_root", key=site)
  }
})
