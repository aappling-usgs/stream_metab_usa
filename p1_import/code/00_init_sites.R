# Parse command-line arguments

args <- commandArgs(TRUE)
for (i in 1:length(args)){
  eval(parse(text=args[i])) # expecting args sb_user, sb_password, and outfile
}
cat(paste0("Setting up site data as ", sb_user, "\n"))


# Load libraries

library(dataRetrieval)
library(mda.streams)
library(sbtools)


# Log in
if(!missing(sb_password)) 
  session = authenticate_sb(sb_user, sb_password) 
else 
  session = authenticate_sb(sb_user)


#' Identify list of sites to import.
#' 
#' For a site to qualify, it must be in ANY of the passed-in stateCd values and
#' have data for ALL of the passed-in p_codes values
identify_nwis_sites <- function(p_codes=c('00010', '00060', '00095', '00300'), stateCd="all") {
  pcodes <- dataRetrieval::readNWISpCode(p_codes)
  cat("Requiring all of the following parameter codes:\n")
  print(pcodes)
  gsub("nwis-", "nwis_", mda.streams::init_nwis_sites(p_codes, stateCd))
}
site_ids <- identify_nwis_sites(p_codes=c('00010', '00060', '00095', '00300'), stateCd=c("wi"))

#' Create site items on SB
#' 
#' Much of the functionality here, which I largely copied from the demo file in
#' mda.streams, is actually in newer functions now. So this function is
#' partially obsolete.
create_SB_sites <- function(site_ids, parent_sb_item='5487139fe4b02acb4f0c8110', session) {

  # Add any items that don't already exist
  for(i in 1:length(site_ids)){
    
    # If the item already exists, skip it
    tmp = sbtools::query_item_identifier('mda_streams','site_root', site_ids[i], session)
    exists <- item_exists("mda_streams", "site_root", site_ids[1], session)
    if(nrow(tmp) > 0) {
      warning('skipping site ', site_ids[i], ', already exists. SBid:', tmp[1,]$id)
      next
    }
    
    # Otherwise, create the item as a child of parent_sb_item
    id = item_create(parent_id = parent_sb_item, title = site_ids[i], session = session)
    item_update_identifier(id, 'mda_streams', 'site_root', site_ids[i], session)
  }
  
  # Check for any items under this parent that aren't in site_ids
  all_items <- item_list_children(parent_sb_item, session=session, limit=10000)
  item_get(id=all_items$id[1])
}
create_SB_sites(site_ids=site_ids, parent_sb_item='5487139fe4b02acb4f0c8110', session)

#' Add time series data to SB

#' Add watershed delineations to SB
