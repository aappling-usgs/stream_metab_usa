source("p1_import/code/process_make_args.R")
args <- process_make_args(c("sb_user", "sb_password", "on_exists", "verbose"))

#' Pull data from NLDAS onto ScienceBase
#' 
#' Include data for all variables with src="NLDAS" in var_codes, all sites
#' currently on SB, and the date range specified
add_nldas_data <- function(on_exists="stop", verbose=TRUE) {
  # identify the data to download
  vars <- get_var_codes() %>% filter(src=="nldas") %>% .$var
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
      if(verbose) message("# variable: ", var)
      
      # refresh site_group list; may be changed on any var in vars
      sites_to_get <- site_group
      
      if(on_exists == "skip") {
        if(verbose) message("checking for existing data...")
        ts_locs <- locate_ts(var_src=make_var_src(var, "nldas"), site_name=sites_to_get)
        sites_to_get <- sites_to_get[is.na(ts_locs)]
      }
      
      if(length(sites_to_get) > 0) {
        lapply(sites_to_get, function(stg) {
          file <- stage_nldas_ts(sites=stg, var=var, times=times, verbose=verbose)
          post_ts(file, on_exists=on_exists, verbose=verbose)
        })
      } else {
        if(verbose) message("no data to acquire or post in this group.")
      }
    }
  }
  
  invisible()
}
add_nldas_data(on_exists=args$on_exists, verbose=args$verbose)