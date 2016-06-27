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
#'  \item{"time.st"}{Start datetime for timeseries, same for all files}
#'  \item{"time.en"}{End datetime for timeseries, same for all files}
#' }
#' \code{time.st} and \code{time.en} are included in the table for two reasons: 
#' (1) ensure that each row of the table contains a full description of each
#' dataset, and (2) break the direct dependency of each post_ts operation on the
#' original ts_config.yml so that changing other parts of ts_config don't force
#' a re-stage and re-post of all the data.
#' 
#' @param sites vector of SB site IDs
#' @param config ts config file
#' @param outfile file to which the table should be written
create_ts_table <- function(sites, config, outfile){
  var <- tail(strsplit(outfile,'[_.]')[[1]],3)[1]
  src <- tail(strsplit(outfile,'[_.]')[[1]],2)[1]
  ts.name <- make_ts_name(var, src)
  filepaths <- make_ts_path(sites, ts.name, version = config$version, folder = config$temp_dir)
  false.vect <- rep(FALSE, length(filepaths))
  time.st <- rep(config$times[1], length(filepaths))
  time.en <- rep(config$times[2], length(filepaths))
  site.table <- data.frame(filepath=filepaths, local=false.vect, remote=false.vect, no.data=false.vect, 
                           time.st=time.st, time.en=time.en)
  write_site_table(site.table, outfile)
  return(outfile)
}
write_site_table <- function(table, filename){
  write.table(table, file=filename, sep='\t', row.names=FALSE)
}
