source("p1_import/code/process_make_args.R")
args <- process_make_args(c("sb_user", "sb_password", "var", "on_exists", "verbose"))

#' Pull data from NWIS onto ScienceBase
#' 
#' Include data for all variables with src="NWIS" in var_codes, all sites
#' currently on SB, and the date range specified
add_styx_data <- function(var="doobs", on_exists="skip", sb_user, sb_password, verbose=TRUE) {
  
  # create the sites
  sites_styx001 <- sprintf("styx_001%03d", 13)
  system.time(posted_sites <- post_site(sites_styx001, on_exists=on_exists, verbose=verbose))
  
  # find a good real-data site to start from
  #   dat <- read_ts(download_ts("wtr_nwis", sites[140], on_local_exists="skip"))
  #   sumry <- dat %>% v() %>% mutate(Date=as.Date(DateTime)) %>% group_by(Date) %>% summarize(wtr_min=min(wtr), wtr_max=max(wtr), wtr_range=max(wtr)-min(wtr))
  #   which(sumry$wtr_min > 9 & sumry$wtr_min < 11 & sumry$wtr_range > 4 & sumry$wtr_range < 7)
  #   ggplot(gather(sumry, type, val, 2:4), aes(x=Date, y=val, color=type)) + geom_line()
  
  # sites(140) = nwis_02207135, dataRetrieval::readNWISsite("02207135"), 
  # sumry[which(sumry$wtr_min > 9 & sumry$wtr_min < 11 & sumry$wtr_range > 4 & 
  # sumry$wtr_range < 7),] gives 12, including 3 in a row on 2014-03-9 to
  # 2014-03-11
  
  # start with real water temperature and coord data
  var <- "wtr_nwis"
  real_site <- "nwis_02207135"
  times <- data.frame(start_date=as.POSIXct("2014-03-08 22:30:00", tz="UTC"), 
                      end_date=as.POSIXct("2014-03-12 06:00:00", tz="UTC"), stringsAsFactors = FALSE)
  if(verbose) {
    message("will get data for these parameter codes: ", paste0(vars, collapse=", "))
    message("will get data between these dates: ", paste0(times, collapse=", "))
    message("will get data for site ", site)
  }
  
  
  # clean up
  message("styx data are fully posted to SB. model away!")
  writeLines(as.character(Sys.time()), sprintf("p1_import/out/is_ready_styx_%s.txt", var))
  invisible()
}
add_styx_data(var=args$var, on_exists=args$on_exists, sb_user=args$sb_user, sb_password=args$sb_password, verbose=args$verbose)
