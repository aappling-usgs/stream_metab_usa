#' Produce a plot and table giving the overall status of ts data staging/posting
#' 
#' @param progress.tsv the tab separated file to write the progress status to
#' @param progress.md the markdown file to write the progress status to
#' @param progress.png the filename for the png output
#' @import mda.streams
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @seealso read_status_table write_status_table
summarize_ts_tables <- function(progress.tsv, progress.md = '../1_timeseries/out/all_ts_files.md', progress.png = '../1_timeseries/out/all_ts_files.png') {
  # change dirs if needed so this function can be run outside of remake
  wd <- getwd()
  if(!(basename(wd) %in% c('remake','stream_metab_usa')))
    stop('current dir must be stream_metab_usa or stream_metab_usa/remake')
  with_chdir <- (basename(wd) == 'stream_metab_usa')
  if(with_chdir) setwd('remake')
  tryCatch({ 
    # load libraries & helpers
    library(mda.streams)
    library(dplyr)
    library(tidyr)
    library(ggplot2)
    source('../lib/write_status_table.R')
    
    # load and combine status info from the var_src-specific files
    tbl_files <- dir('../1_timeseries/out', full.names=TRUE) %>% {.[!grepl('all_ts_files|Thumbs.db', .)]}
    tbls <- lapply(tbl_files, read_status_table)
    all_var_srces <- c(
      'doobs_nwis', 'wtr_nwis', 'disch_nwis', 
      'baro_nldas', 'sw_nldas', 'baro_gldas', 'sw_gldas', 
      'dosat_calcGGbts', 'baro_calcElev', 'dosat_calcGGbconst', 'dopsat_calcObsSat', 
      'depth_calcDischRaymond', 'veloc_calcDischRaymond', 'depth_calcDischHarvey', 'veloc_calcDischHarvey', 
      'sitetime_calcLon', 'suntime_calcLon', 'par_calcLat', 'par_calcSw', 'par_calcLatSw', 
      'sitedate_calcLon', 'doamp_calcDAmp', 'dischdaily_calcDMean', 'velocdaily_calcDMean')
    progress_bars <- bind_rows(lapply(tbls, function(tbl) {
      summarize(
        tbl, 
        var_src = ordered(parse_ts_path(filepath[1], out='var_src'), all_var_srces),
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
    if(length(unique(progress_bars$barheight)) != 1) warning("couldn't reconcile barheights")
    progress_bars <- progress_bars %>%
      full_join(tibble(var_src=ordered(levels(progress_bars$var_src),levels(progress_bars$var_src))), by='var_src') %>%
      arrange(var_src)
    
    # write a table
    write_status_table(progress_bars, progress.tsv)
    
    # write a pretty table
    writeLines(knitr::kable(progress_bars), progress.md)
    
    # make a plot
    bardata <- progress_bars %>% 
      gather(status, value, uncounted, untouched, no_data, local, posted_untagged, posted_tagged) %>%
      mutate(status = factor(status, c('uncounted', 'untouched', 'no_data', 'local', 'posted_untagged', 'posted_tagged')))
    g <- ggplot(bardata, aes(x=var_src)) + 
      geom_bar(aes(y=value, fill=status), position='stack', stat='identity') +
      ylab('Number of sites') + xlab('') +
      theme_bw() + theme(axis.text.x=element_text(angle=90))
    ggsave(progress.png, plot=g, width=8, height=8, units='in')
    
  }, finally = { if(with_chdir) setwd(wd) }
  )
  progress_bars
}
