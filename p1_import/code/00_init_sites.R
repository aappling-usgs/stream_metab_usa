source("p1_import/code/process_make_args.R")
args <- process_make_args(c("sb_user", "sb_password", "outfile", "update_sitelist","replace_existing", "delete_all", "verbose"))

init_sites <- function(update_sitelist=TRUE, replace_existing=FALSE, delete_all=FALSE, verbose=TRUE) {
  # delete everything before starting (optional)
  if(delete_all) {
    if(verbose) message("deleting all sites on ScienceBase")
    sites_sb <- get_sites()
    mda.streams:::delete_site(sites_sb, verbose=verbose) 
  }
  
  # get sitelist from NWIS (optional)
  if(update_sitelist) {
    if(verbose) message("acquiring site list from NWIS server")
    sites_file <- stage_nwis_sitelist(p_codes=get_var_codes("doobs","p_code"), state_codes=c("all"), folder="p1_import/doc", verbose=verbose)
  } else {
    sites_file <- "p1_import/doc/nwis_sites.txt"
  }
  
  # post/repost sites to SB
  sites_nwis <- readLines(sites_file)
  if(verbose) message("posting sites with replace_existing==",replace_existing)
  posted_sites <- post_site(sites_nwis, replace_existing=replace_existing, verbose=verbose)
  
  # print results & return
  if(verbose) {
    sites_display <- 
      data.frame(site=names(posted_sites), posted_id=posted_sites, stringsAsFactors=FALSE) %>%
      transmute(site, existing_id=ifelse(is.na(posted_id), locate_site(site), NA), posted_id)
    print(sites_display)
  }
  return()
}
init_sites(update_sitelist=args$update_sitelist, replace_existing=args$replace_existing, delete_all=args$delete_all, verbose=args$verbose)
