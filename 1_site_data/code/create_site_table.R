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
#' }
#' 
#' @param config ts config file
#' @param outfile file to which the table should be written
create_site_table <- function(config, outfile){

  # combine existing SB sites with all NWIS sites meeting our criteria
  project.sites <- mda.streams::list_sites()
  fresh.sites <- mda.streams::stage_nwis_sitelist(
    vars=config$has.vars, min.obs=config$min.count, site.types=config$site.types, 
    HUCs=1:21, folder=NULL, verbose=TRUE)
  sites <- union(project.sites, fresh.sites)
  
  # create the site table
  site.table <- data.frame(
    site_name=sites,
    remote=FALSE,
    no.data=sites %in% fresh.sites)

  write_status_table(site.table, outfile)
  
  return()
}
