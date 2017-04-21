library(mda.streams)
library(dplyr)
stage_ts_metab <- function(mm, config=read_config('../2_metab_models/run3/out/config.tsv'), folder='../2_metab_outputs/out/ts') {
  
  if(!dir.exists(folder)) dir.create(folder)
  
  # let mm be either a metabolism model or the filepath to an Rdata metabolism model
  if(is(mm, 'character')) mm <- get(load(mm))
  
  # place this mm in the context of all mms for this site
  config <- config %>%
    mutate(
      resolution = substring(strategy, 7),
      model_name = make_metab_model_name(title=make_metab_run_title(date=format(as.Date(date), '%y%m%d'), tag=tag, strategy=strategy), row=config.row, site=site))
  mm_config <- get_info(mm)$config %>%
    mutate(
      resolution = substring(strategy, 7),
      model_name = make_metab_model_name(title=make_metab_run_title(date=format(as.Date(date), '%y%m%d'), tag=tag, strategy=strategy), row=config.row, site=site))
  site_mms <- filter(config, site==mm_config$site)
  
  # write files for the current model=resolution (could be several resolutions per site)
  tsfiles <- stage_metab_ts(mm, folder=folder)
  sapply(tsfiles, function(tsfile) {
    newpath <- file.path(paste0(tools::file_path_sans_ext(tsfile), '_', mm_config$resolution, '.', tools::file_ext(tsfile)))
    file.copy(tsfile, newpath)
    file.remove(tsfile)
  })
  
  # if we're ready, combine all the ts files for this site for each param
  parfiles <- grep(paste0(mm_config$site, '.*estBest\\.'), dir(folder, full.names=TRUE), value=TRUE)
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
      write_ts(bigvardf, site=stdparse$site_name, var=stdparse$var, src=stdparse$src, folder=folder, version='rds')
    })
  }
  
}