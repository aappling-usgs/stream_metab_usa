source("p1_import/code/process_make_args.R")
args <- process_make_args(c("sb_user", "sb_password", "on_exists", "verbose"))

#' Pull data from NLDAS onto ScienceBase
#' 
#' Include data for all variables with src="NLDAS" in var_codes, all sites
#' currently on SB, and the date range specified
add_nldas_data <- function(on_exists="stop", sb_user, sb_password, verbose=TRUE) {
  # identify the data to download
  vars <- get_var_codes() %>% filter(src=="nldas") %>% .$var
  sites <- sort(get_sites())
  times <- unlist(read.table("p1_import/in/date_range.tsv", header=TRUE, stringsAsFactors=FALSE))
  if(verbose) {
    message("will get data for these parameter codes: ", paste0(vars, collapse=", "))
    message("will get data between these dates: ", paste0(times, collapse=", "))
    message("will get data for ", length(sites), " sites")
  }
  
  # break the calls into manageably sized time chunks to avoid incomplete downloads, 
  # which bother geo data portal (and us). ScienceBase probably won't mind the
  # smaller tasks, either.
  times_per_group <- 48
  time_groups <- round(seq(as.POSIXct(times[1], tz = 'UTC'), to=as.POSIXct(times[2], tz = 'UTC'), length.out = times_per_group+1), 'days')
  times <- data.frame(
    time_start=head(time_groups,-1),
    time_end = tail(time_groups,-1),
    group = seq(length.out = times_per_group),
    stringsAsFactors=FALSE)
  
  if(verbose) message("downloading temp data to ", tempdir())
  
  for(var in vars) {
    if(verbose) message("# variable: ", var)
    
    # refresh site_group list; may be changed on any var in vars
    sites_to_get <- sites 
    
    if(on_exists == "skip") {
      if(verbose) message("checking for existing data...")
      ts_locs <- locate_ts(var_src=make_var_src(var, "nldas"), site_name=sites_to_get)
      sites_to_get <- sites_to_get[is.na(ts_locs)]
    }
    if(length(sites_to_get) > 0) {
    # loop through groups and vars to download and post files.
      for(group in unique(times$group)) {
        time_vals <- c(times[times$group==group,"time_start"], times[times$group==group,"time_end"])
        if(verbose) message("\n## time group ", group, ":\n", paste0(time_vals, collapse=", "))
        
        
        files <- stage_nldas_ts(sites=sites_to_get, var=var, times=time_vals, verbose=verbose)
        # reauthenticate if needed
        if(is.null(current_session())) {
          message("\re-authenticating with ScienceBase with the password you set.\n")
          authenticate_sb(sb_user, sb_password) 
        }
        post_ts(files, on_exists=on_exists, verbose=verbose)
      } 
    } else {
      if(verbose) message("no data to acquire or post in this group.")
    }
  
  }
  
  invisible()
}
add_nldas_data(on_exists=args$on_exists, sb_user=args$sb_user, sb_password=args$sb_password, verbose=args$verbose)
