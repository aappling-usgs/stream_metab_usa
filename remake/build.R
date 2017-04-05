#' Remake for SMU (stream_metab_usa)
#' 
#' @examples
#' \dontrun{
#' # put this in your .Rprofile file in the top directory of this project:
#' message('sourcing remake/build.R, 1_timeseries/code/summarize_ts_tables.R')
#' source('remake/build.R')
#' source('1_timeseries/code/summarize_ts_tables.R')
#' }
#' 
#' sbsites <- remake_smu('sb_sites', '1_site_data.yml')
#' remake_smu('../1_timeseries/out/files_ts_wtr_nwis.tsv', '1_timeseries.yml')
#' remake_smu('wtr_nwis', '1_timeseries.yml')
remake_smu <- function(target_names, remake_file, ...) {
  message(Sys.time())
  shortdir <- function(wd=getwd()) file.path(basename(dirname(wd)), basename(wd))
  message('current directory: ', shortdir())
  wd <- getwd()
  if(!(basename(wd) %in% c('remake','stream_metab_usa')))
    stop('current dir must be stream_metab_usa or stream_metab_usa/remake')
  with_chdir <- (basename(wd) == 'stream_metab_usa')
  if(with_chdir) {
    setwd('remake')
    message('running remake from ', shortdir())
  }
  tryCatch(
    { out <- remake::make(target_names=target_names, remake_file=remake_file, ...) },
    finally = { if(with_chdir) setwd(wd) }
  )
  message('current directory: ', shortdir())
  message(Sys.time())
  return(out)
}
