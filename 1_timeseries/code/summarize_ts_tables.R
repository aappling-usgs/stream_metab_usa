#' Produce a plot and table giving the overall status of ts data staging/posting
#' 
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @seealso read_status_table write_status_table
summarize_ts_tables <- function() {
  tbl_files <- dir('../1_timeseries/out', full.names=TRUE) %>% {.[!grepl('all_ts_files', .)]}
  tbls <- lapply(tbl_files, read_status_table)
  progress_bars <- bind_rows(lapply(tbls, function(tbl) {
    summarize(
      tbl, 
      var_src = factor(
        parse_ts_path(filepath[1], out='var_src'),
        c('doobs_nwis', 'wtr_nwis', 'disch_nwis', 
          'baro_nldas', 'sw_nldas', 'baro_gldas', 'sw_gldas', 
          'dosat_calcGGbts', 'baro_calcElev', 'dosat_calcGGbconst', 'dopsat_calcObsSat', 
          'depth_calcDischRaymond', 'veloc_calcDischRaymond', 'depth_calcDischHarvey', 'veloc_calcDischHarvey', 
          'sitetime_calcLon', 'suntime_calcLon', 'par_calcLat', 'par_calcSw', 
          'sitedate_calcLon', 'doamp_calcDAmp', 'dischdaily_calcDMean', 'velocdaily_calcDMean')),
      total = length(filepath),
      untouched = length(which(!no.data & !local & !posted & !tagged)),
      no_data = length(which(no.data)),
      local = length(which(local & !posted)),
      posted_untagged = length(which(posted & !tagged)),
      posted_tagged = length(which(tagged & !no.data))
    )
  })) %>%
    mutate(
      uncounted = max(total)-total,
      barheight = uncounted + untouched + no_data + local + posted_untagged + posted_tagged)
  if(length(unique(progress_bars$barheight)) != 1) stop("couldn't reconcile barheights")
  
  # write a table
  write_status_table(progress_bars, '../1_timeseries/out/all_ts_files.tsv')
  
  # write a pretty table
  writeLines(kable(progress_bars), '../1_timeseries/out/all_ts_files.md')
  
  # make a plot
  bardata <- progress_bars %>% 
    gather(status, value, uncounted, untouched, no_data, local, posted_untagged, posted_tagged) %>%
    mutate(status = factor(status, c('uncounted', 'untouched', 'no_data', 'local', 'posted_untagged', 'posted_tagged')))
  g <- ggplot(bardata, aes(x=var_src)) + 
    geom_bar(aes(y=value, fill=status), position='stack', stat='identity') +
    ylab('Number of sites') + xlab('') +
    theme_bw() + theme(axis.text.x=element_text(angle=90))
  ggsave('../1_timeseries/out/all_ts_files.png', plot=g, width=8, height=8, units='in')
}
