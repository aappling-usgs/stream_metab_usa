combine_daily_predictors <- function(out_file, metab_config) {
  daily_preds <- dplyr::bind_rows(lapply(metab_config$site, function(site) {
    ts_files <- paste0('../1_timeseries/cache/', site, '-ts_', c('sitedate_calcLon','doamp_calcDAmp','dischdaily_calcDMean','velocdaily_calcDMean'), '.rds')
    if(any(!file.exists(ts_files))) {
      stop(paste("ts files must be downloaded locally to combine_daily_predictors:", paste(ts_files[!file.exists(ts_files)], collapse=', ')))
    }
    ts_df <- readRDS(ts_files[1])
    for(tsfile in ts_files[-1]) {
      ts_df <- dplyr::left_join(ts_df, readRDS(tsfile), by='DateTime')
    }
    unitted::v(dplyr::mutate(ts_df, site=site))
  }))
  saveRDS(daily_preds, out_file)
}
