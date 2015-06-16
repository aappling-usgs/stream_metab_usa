source("p1_import/code/process_make_args.R")
args <- process_make_args(c("sb_user", "sb_password", "outfile", "on_exists", "verbose"))

#' Pull data from NWIS onto ScienceBase
#' 
#' Include data for all variables with src="NWIS" in var_codes, all sites
#' currently on SB, and the date range specified
add_nwis_data <- function(on_exists="stop", verbose=TRUE) {
  # identify the data to download
  vars <- get_var_codes() %>% filter(src=="nwis") %>% .$var
  sites <- sort(get_sites())[1:3]
  times <- unlist(read.table("p1_import/in/date_range.tsv", header=TRUE, stringsAsFactors=FALSE))
  if(verbose) {
    message("will get data for these parameter codes: ", paste0(vars, collapse=", "))
    message("will get data between these dates: ", paste0(times, collapse=", "))
    message("will get data for ", length(sites), " sites")
  }
  
  # break the sites into manageably sized groups to avoid incomplete downloads, 
  # which bother dataRetrieval (and us). ScienceBase probably won't mind the
  # smaller tasks, either.
  sites_per_group <- 10
  sites <- data.frame(
    site=sites, 
    group=rep(1:ceiling(length(sites)/sites_per_group), each=sites_per_group)[1:length(sites)], 
    stringsAsFactors=FALSE)
  
  # loop through groups and vars to download and post files.
  for(group in unique(sites$group)) {
    site_group <- sites[sites$group==group,"site"]
    if(verbose) message("\nsite group ", group, ":\n", paste0(site_group, collapse=", "))
    
    for(var in vars) {
      if(verbose) message("  variable: ", var)
      
      if(verbose) message("    checking for existing data...")
      ts_summaries <- summarize_ts(var_src=make_var_src(var, "nwis"), site_name=site_group)
      sites_new <- site_group[is.na(ts_summaries$id)]
      
      if(length(sites_new) > 0) {
        if(verbose) message("    acquiring data...")
        files <- stage_nwis_ts(sites=sites_new, var=var, times=times, verbose=verbose)
        
        if(verbose) message("    posting data...")
        post_ts(files, on_exists=on_exists, verbose=verbose)
        
      } else {
        if(verbose) message("    no data to acquire or post in this group.")
      }      
    }
  }
}
add_nwis_data(on_exists=args$on_exists, verbose=args$verbose)