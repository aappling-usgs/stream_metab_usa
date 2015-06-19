source("p1_import/code/process_make_args.R")
args <- process_make_args(c("sb_user", "sb_password", "var", "on_exists", "verbose"))

#' Pull data from NWIS onto ScienceBase
#' 
#' Include data for all variables with src="NWIS" in var_codes, all sites
#' currently on SB, and the date range specified
add_nwis_data <- function(var="doobs", on_exists="stop", sb_user, sb_password, verbose=TRUE) {
  # identify the data to download
  vars <- var # get_var_codes() %>% filter(src=="nwis") %>% .$var
  sites <- sort(get_sites())
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
  
  if(verbose) message("downloading temp data to ", tempdir())
  
  # loop through groups and vars to download and post files.
  for(group in unique(sites$group)) {
    site_group <- sites[sites$group==group,"site"]
    if(verbose) message("\n## site group ", group, ":\n", paste0(site_group, collapse=", "))
    
    for(var in vars) {
      #if(verbose) message("# variable: ", var)
      
      # refresh site_group list; may be changed on any var in vars
      sites_to_get <- site_group
      
      if(on_exists == "skip") {
        if(verbose) message("checking for existing data...")
        ts_locs <- locate_ts(var_src=make_var_src(var, "nwis"), site_name=sites_to_get)
        sites_to_get <- sites_to_get[is.na(ts_locs)]
      }
      
      # download and post the data. have backed away from multiple sites at 
      # once; often files get too big. also, alternating NWIS and SB helps
      # reduce strain on both.
      if(length(sites_to_get) > 0) {
        for(stg in sites_to_get) {
          # reauthenticate if needed
          if(is.null(current_session())) {
            message("\re-authenticating with ScienceBase with the password you set.\n")
            authenticate_sb(sb_user, sb_password) 
          }
          tryCatch({
            file <- stage_nwis_ts(sites=stg, var=var, times=times, verbose=verbose)
            post_ts(file, on_exists=on_exists, verbose=verbose)
          }, error=function(e) {
            message("stage or post operation failed for ", stg, "; skipping ts")
            message(e)
          })
        }
      } else {
        if(verbose) message("no data to acquire or post in this group.")
      }
    }
  }
  
  message("NWIS data are fully posted to SB for var=", var)
  writeLines(as.character(Sys.time()), "p1_import/out/is_ready_nwis_doobs.txt")
  invisible()
}
add_nwis_data(var=args$var, on_exists=args$on_exists, sb_user=args$sb_user, sb_password=args$sb_password, verbose=args$verbose)