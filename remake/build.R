#' Remake for SMU (stream_metab_usa)
#' 
#' If you set your Build | "Custom build script" to this file (enter 
#' 'remake/build.R' in the text box), then it will open right away in your
#' editor pane.
#' 
#' @examples
#' sbsites <- remake_smu('sb_sites', '1_site_data.yml')
#' remake_smu('../1_timeseries/out/files_ts_wtr_nwis.tsv', '1_timeseries.yml')
#' remake_smu('wtr_nwis', '1_timeseries.yml')
remake_smu <- function(target_names, remake_file, ...) {
  wd <- getwd()
  message('current directory: ', wd)
  if(!(basename(wd) %in% c('remake','stream_metab_usa')))
    stop('current dir must be stream_metab_usa or stream_metab_usa/remake')
  with_chdir <- (basename(wd) == 'stream_metab_usa')
  if(with_chdir) {
    setwd('remake')
    message('running remake from ', getwd())
  }
  tryCatch(
    { out <- remake::make(target_names=target_names, remake_file=remake_file, ...) },
    finally = { if(with_chdir) setwd(wd) }
  )
  message('current directory: ', wd)
  return(out)
}
