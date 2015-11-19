source("p1_import/code/process_make_args.R")
args <- process_make_args(c("sb_user", "sb_password", "var_src", "on_exists", "verbose"))

#' Pull data from calc onto ScienceBase
#' 
#' Include data for all variables with src="calc" in var_codes, all sites
#' currently on SB, and the date range specified
add_calc_data <- function(var_src="suntime_calcLon", sites=list_sites(), on_exists="stop", sb_user, sb_password, verbose=TRUE) {
  # identify the data to download
  
  if(length(var_src) != 1) stop("expecting exactly 1 var_src")
  var <- parse_var_src(var_src)$var
  src <- parse_var_src(var_src)$src
  if(verbose) {
    message("will get data for this var_src: ", var_src)
    message("will calculate data for up to ", length(sites), " sites")
    message("will download temp data to ", tempdir())
  }

  # determine list of sites we ACTUALLY need to compute
  sites_to_get <- sites
  if(on_exists %in% c("stop", "skip")) {
    if(verbose) message("checking for existing data...")
    ts_locs <- locate_ts(var_src=var_src, site_name=sites_to_get)
    if(on_exists == "stop" && length(ts_already <- which(!is.na(ts_locs))) > 0)
      stop("on_exists='stop' and ", length(ts_already), " sites already have data: ", paste0(sites[ts_already], collapse=","))
    sites_to_get <- sites_to_get[is.na(ts_locs)]
  }
    
  # loop through groups and var to download and post files.
  for(site in sites_to_get) {

    # reauthenticate if needed
    if(is.null(current_session())) {
      message("re-authenticating with ScienceBase with the password you set.\n")
      authenticate_sb(sb_user, sb_password) 
    }
    
    # download inputs, calculate data, and post.
    tryCatch({
      ts_file <- stage_calc_ts(sites=site, var=var, src=src, verbose=TRUE)
      post_ts(ts_file, on_exists=on_exists, verbose=TRUE)
    }, error=function(e) {
      message("stage or post operation failed for ", site, "; skipping ts")
      message(e)
    })
  }
  
  message("calc data are fully posted to SB for var_src=", var_src)
  writeLines(as.character(Sys.time()), sprintf("p1_import/out/is_ready_calc_%s.txt", var_src))
  invisible()
}
add_calc_data(var_src=args$var_src, sites=list_sites(), on_exists=args$on_exists, sb_user=args$sb_user, sb_password=args$sb_password, verbose=args$verbose)
