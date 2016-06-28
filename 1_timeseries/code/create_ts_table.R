#' Create a file to record timeseries stage/post status
#' 
#' Creates a tsv file for recording whether a ts file exists on NWIS/LDAS,
#' whether it's been successfully downloaded, and whether it's been successfully
#' posted.
#' 
#' @section columns:
#' \itemize{
#'  \item{"filepath"}{Local path where timeseries should be staged before 
#'  posting. This is unchanged in the stage and post phases. Also used by
#'  \code{post_ts()} to determine the file name where the timeseries is posted
#'  on ScienceBase.}
#'  \item{"local"}{Starts as FALSE. If it can be confirmed in \code{stage_ts()} 
#'  that the file has been successfully downloaded to the local temp directory,
#'  this is set to TRUE in the row for that file.}
#'  \item{"remote"}{Starts as FALSE. If it can be confirmed in
#'  \code{sb_post_ts()} that the file has been successfully posted to
#'  ScienceBase, this is set to TRUE in the corresponding row.}
#'  \item{"no.data"}{Starts as FALSE. If it is discovered in \code{stage_ts()} 
#'  that there are no remote data on NWIS/NLDAS/GLDAS available for a site, this
#'  is set to TRUE in the corresponding row.}
#' }
#' 
#' @param sites vector of SB site IDs
#' @param config ts config file
#' @param outfile file to which the table should be written
create_ts_table <- function(sites, config, outfile){
  var <- tail(strsplit(outfile,'[_.]')[[1]],3)[1]
  src <- tail(strsplit(outfile,'[_.]')[[1]],2)[1]
  ts.name <- make_ts_name(var, src)
  if(!dir.exists(config$temp_dir)) dir.create(config$temp_dir)
  filepaths <- make_ts_path(sites, ts.name, version = config$version, folder = config$temp_dir)
  false.vect <- rep(FALSE, length(filepaths))
  site.table <- data.frame(filepath=filepaths, local=false.vect, remote=false.vect, no.data=false.vect)
  write_status_table(site.table, outfile)
  return(outfile)
}
