stage_ts_metab <- function(ts.file, outputs.config, metab.config) {
  
  # update the ts.file for remote and local status. done here in case the status
  # table was last updated on someone else's computer
  to.stage <- sb_check_ts_status(ts.file, phase='stage') # not used by 'stage': posted_after=config$posted_after
  if(nrow(to.stage) == 0) {
    return(TRUE) # if we're already done, return now
  }
  to.stage <- bind_cols(to.stage, parse_ts_path(to.stage$filepath, out=c('site_name','version','dir_name')))
  
  # read the full ts.table for reporting (and, for calc_ts, documenting progress)
  ts.table <- read_status_table(ts.file)
  message(
    'staging data for ', nrow(to.stage), ' new sites; ', 
    nrow(ts.table) - nrow(to.stage),' are already local/posted/unavailable; ', 
    nrow(ts.table), ' sites total')
  
  # make sure local staging dir exists
  dir.name <- unique(to.stage$dir_name)
  lapply(dir.name, function(x) if(!dir.exists(x)) dir.create(x))
  
  # determine which function to call and for which sites
  var <- tail(strsplit(ts.file,'[_.]')[[1]],3)[1]
  src <- tail(strsplit(ts.file,'[_.]')[[1]],2)[1]
  
  # combine the prediction tses from all the models for this site
  for (i in 1:nrow(to.stage)) {
    site_name <- to.stage$site_name[i]
    site_mms <- dplyr::filter(metab.config, site==site_name)
    site_res <- substring(site_mms$strategy, 7)
    
    # if we're ready, combine all the ts files for this site for each param
    varfiles <- grep(sprintf('%s-ts_%s_estBest_', site_name, var), dir('../2_metab_outputs/out/tses', full.names=TRUE), value=TRUE)
    if(all(sapply(site_res, function(sr) any(grepl(sr, varfiles))))) {
      vardfs <- lapply(varfiles, function(vf) as.data.frame(readRDS(vf)) %>% {.[complete.cases(.),]})
      bigvardf <- do.call(rbind, vardfs) %>% .[order(.$DateTime),]
      
      # determine the new file name and write the file
      res <- strsplit(tools::file_path_sans_ext(basename(varfiles[1])), '_')[[1]][5]
      stdname <- gsub(paste0('_',res), '', varfiles[1])
      stdparse <- parse_ts_path(stdname)
      write_ts(bigvardf, site=stdparse$site_name, var=stdparse$var, src=stdparse$src, folder=dir.name, version=outputs.config$version)
    } else {
      warning("missing one or more model-specific files for site ", site_name, "; skipping final ts file for this site")
    }
  }
  
  # update and evaluate the staging status
  to.stage <- sb_check_ts_status(ts.file, phase='stage')
  num_incomplete <- nrow(to.stage)
  if(is.numeric(num_incomplete) && num_incomplete==0) {
    return(TRUE)
  } else {
    stop("staging was incomplete; ", num_incomplete, " ts files still to stage")
  }
}
