#' helper function for posting files to sciencebase w/ post_ts
#' 
#' @param files files created by stage_ts (or stage_nwis_ts; stage_nldas_ts)
sb_post_ts <- function(ts.file){
  
  ts.config <- yaml.load_file("../1_timeseries/in/ts_config.yaml")
  auth_from_profile()
  
  ts.table <- read.table(file=ts.file, sep='\t', header = TRUE, stringsAsFactors = FALSE)
  details <- parse_ts_path(ts.table$filepath, out=c('site_name', 'var_src','version'))
  remote.sites <- list_sites(unique(details$var_src), with_ts_version=unique(details$version))
  remote <- ts.table$remote | details$site_name %in% remote.sites
  ts.table$remote <- remote
  
  files <- ts.table$filepath[ts.table$local & !ts.table$remote]
  
  for (file in files){
    site <- parse_ts_path(file, out='site_name', use_names = FALSE)
    # if(is.na(locate_site(site, by = 'tag'))){
    #   post_site(site, on_exists = "skip", verbose=TRUE)
    # } # this should be handled by sb_post_sites now
    sb.id <- post_ts(file, on_exists=ts.config$on_exists, verbose=TRUE)
    if (is.character(sb.id) & nchar(sb.id) > 0){
      file.i <- which(file == ts.table$filepath)
      ts.table$remote[file.i] <- TRUE
      write_status_table(ts.table, ts.file)
    }
  }
  remote.sites <- parse_ts_path(ts.table$filepath[ts.table$remote], out='site_name', use_names = FALSE)
  return(remote.sites)
}
