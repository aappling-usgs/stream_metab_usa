library(mda.streams)
library(dplyr)
library(streamMetabolizer)
stage_ts_metab <- function(site, config, outdir='../2_metab_outputs/out/ts') {
  
  if(!dir.exists(outdir)) dir.create(outdir)
  
  # place this mm in the context of all mms for this site
  mm_config <- config %>%
    filter(config.row == get_info(mm)$config$config.row)
  site_mms <- filter(config, site==mm_config$site)
  
  # write files for the current model=resolution (could be several resolutions per site)
  tsfiles <- stage_metab_ts(mm, folder=outdir)
  sapply(tsfiles, function(tsfile) {
    newpath <- file.path(paste0(tools::file_path_sans_ext(tsfile), '_', mm_config$resolution, '.', tools::file_ext(tsfile)))
    file.copy(tsfile, newpath)
    file.remove(tsfile)
  })
  
  # if we're ready, combine all the ts files for this site for each param
  parfiles <- grep(paste0(mm_config$site, '.*estBest\\.'), dir(outdir, full.names=TRUE), value=TRUE)
  if(length(parfiles) == 3*nrow(site_mms)) {
    sapply(c('gpp','er','K600'), function(var) {
      # collect and combine the existing files into one big data.frame
      varfiles <- grep(paste0(var, '_estBest_'), parfiles, value=TRUE)
      vardfs <- lapply(varfiles, function(vf) as.data.frame(readRDS(vf)))
      bigvardf <- do.call(mda.streams:::combine_ts, c(vardfs, list(method='full_join'))) %>%
        .[order(.$DateTime),] # necessary? not sure
      
      # determine the new file name and write the file
      res <- strsplit(tools::file_path_sans_ext(basename(varfiles[1])), '_')[[1]][5]
      stdname <- gsub(paste0('_',res), '', varfiles[1])
      stdparse <- parse_ts_path(stdname)
      write_ts(bigvardf, site=stdparse$site_name, var=stdparse$var, src=stdparse$src, folder=outdir, version='rds')
    })
  }
  
}