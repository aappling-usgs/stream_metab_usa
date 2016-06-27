#' Create a file to record site stage/post status
#' 
#' Creates a tsv file for recording whether a site exists on NWIS and/or SB and
#' whether it's been successfully posted to SB.
#' 
#' @section columns:
#' \itemize{
#'  \item{"site_name"}{Site name for which to post an SB item using 
#'  \code{post_site()}.}
#'  \item{"remote"}{Starts as FALSE. If it can be confirmed in 
#'  \code{sb_post_site()} that the item has been successfully posted to 
#'  ScienceBase, this is set to TRUE in the corresponding row.}
#'  \item{"no.data"}{Starts as FALSE. If it is discovered in 
#'  \code{create_site_table()} that the site criteria are not currently met on 
#'  NWIS for this site, this is set to TRUE in the corresponding row. Thus this 
#'  column is a record of whether a site was substantially more available on 
#'  NWIS in the past and/or was added manually, as in a 'styx' or 'indy' site. 
#'  Sites will \emph{not} be deleted from ScienceBase even if no.data becomes 
#'  TRUE.}
#'  \item{"min.count"}{The minimum number of observations a site must have to be
#'  added to the site table, same for all files}
#'  \item{"skip.types"}{NWIS site types (site_tp_cd) to omit, same for all
#'  files}
#'  \item{"has.param"}{NWIS parameter code[s] (parm_cd) to require, same for all
#'  files}
#' }
#' \code{min.count}, \code{skip.types}, and \code{has.param} are included in the
#' table for two reasons: (1) ensure that each row of the table contains a full
#' description of each site, and (2) break the direct dependency of each
#' post_site operation on the original sites_config.yaml so that changing other
#' parts of sites_config.yaml don't force a re-query and re-post of all the
#' sites.
#' 
#' @param config ts config file
#' @param outfile file to which the table should be written
create_site_table <- function(config, outfile){
  sites <- init_site_list(config, outfile)

  site.table <- data.frame(
    site_name=sites, 
    remote=FALSE, 
    no.data=FALSE,
    min.count=config$min.count,
    skip.types=paste(config$skip.types, collapse=';'),
    has.param=paste(config$has.param, collapse=';'))

  write_status_table(site.table, outfile)
  
  return()
}
