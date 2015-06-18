source("p1_import/code/process_make_args.R")
args <- process_make_args(c("sb_user", "sb_password", "outfile", "update_sitelist","on_exists", "delete_all", "verbose"))

init_sites <- function(update_sitelist=TRUE, on_exists='clear', delete_all=FALSE, verbose=TRUE) {
  # determine the baseline
  sites_sb <- get_sites()

  # delete everything before starting (optional)
  if(delete_all) {
    if(verbose) message("deleting all sites on ScienceBase")
    mda.streams:::delete_site(sites_sb, verbose=verbose) 
  }
  
  # get sitelist from NWIS (optional)
  if(update_sitelist) {
    if(verbose) message("acquiring site list from NWIS server")
    sites_file <- stage_nwis_sitelist(vars="doobs", state_codes=c("all"), folder="p1_import/doc", verbose=verbose)
  } else {
    sites_file <- "p1_import/doc/nwis_sitelist.txt"
  }
  
  # post/repost sites to SB
  sites_nwis <- readLines(sites_file)
  if(verbose) message("posting sites with on_exists==",on_exists)
  posted_sites <- post_site(sites_nwis, on_exists=on_exists, verbose=verbose)
  
  # print results
  if(verbose) {
    message("posted sites:")
    sites_display <- 
      data.frame(site=names(posted_sites), posted_id=posted_sites, stringsAsFactors=FALSE) %>%
      transmute(site, existing_id=ifelse(names(posted_sites) %in% sites_sb, locate_site(site), NA), posted_id)
    print(sites_display)
  }
  
  # check for sites that aren't in the new sitelist (but just print, don't delete)
  sites_sb <- get_sites()
  if(verbose) {
    message("sites that are on SB but not in the nwis sitelist:")
    cat(paste0(setdiff(sites_sb, sites_nwis), collapse="\n"))
  }
  
  return()
}
init_sites(update_sitelist=args$update_sitelist, on_exists=args$on_exists, delete_all=args$delete_all, verbose=args$verbose)
