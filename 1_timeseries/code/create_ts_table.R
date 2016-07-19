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
#' 
#' @seealso write_status_table
create_ts_table <- function(sites, config, outfile){
  
  # Only create and update a fresh table if no current one exists. This fixes 
  # the problem where the table was getting created here, updated in stage_ts, 
  # and then overwritten by a re-creation because remake notices that the file 
  # is now different from its hashed file. And stage_ts and sb_post_ts do plenty
  # of status checking themselves, so no need even to check the status here if
  # the file already exists.
  if(!file.exists(outfile)) {
    # create the filepaths appropriate to the var and src implied by outfile
    var <- tail(strsplit(outfile,'[_.]')[[1]],3)[1]
    src <- tail(strsplit(outfile,'[_.]')[[1]],2)[1]
    ts.name <- make_ts_name(var, src)
    if(!dir.exists(config$temp_dir)) dir.create(config$temp_dir)
    filepaths <- make_ts_path(sites, ts.name, version = config$version, folder = config$temp_dir)
    
    if(grepl("^calc", src)) {
      # figure out the dependencies specific to this var_src and their availability
      no_data <- create_calc_ts_nodata_table(sites, var, src)
    } else {
      # assume there's data on NWIS/LDAS until & unless stage_ts discovers otherwise
      no_data <- data.frame(no.data=FALSE)
    }
    
    # create and write the site table
    site.table <- data.frame(filepath=filepaths, local=FALSE, no_data, posted=FALSE, tagged=FALSE)
    write_status_table(site.table, outfile)
    
    # udpate the local/posted/tagged columns
    sb_check_ts_status(outfile, phase='stage')
    sb_check_ts_status(outfile, phase='post', posted_after=config$posted_after)
  }
  
  return()
}

#' Each calculated ts variable has a different set of data dependencies; create
#' a column for each dependency, plus a final column for whether any
#' dependencies are unavailable (no.data)
#' 
#' @import dplyr
#' @import mda.streams
create_calc_ts_nodata_table <- function(sites, var, src) {
  # look up the dependencies and format them into a list of column names
  calc_ts_needs <- build_calc_ts_needs(var=var, src=src)
  var_choices <- strsplit(calc_ts_needs$var_src_needs, ' ')[[1]] %>%
    strsplit('\\|') %>%
    { setNames(., lapply(., function(var_opts) paste0('var.', unique(parse_var_src(var_opts, out='var'))))) }
  needs_cols <- c(
    if(calc_ts_needs$var_src_needs != '')paste0('var.', parse_var_src(strsplit(calc_ts_needs$var_src_needs, ' ')[[1]], out='var')),
    if(calc_ts_needs$coord_needs != '') paste0('coord.', strsplit(calc_ts_needs$coord_needs, ' ')[[1]]),
    if(calc_ts_needs$dvq_needs != '') paste0('dvqcoef.', strsplit(calc_ts_needs$dvq_needs, ' ')[[1]]))
  
  # create a df of logicals for which sites have which data dependencies
  if(calc_ts_needs$coord_needs != '') {
    coords <- get_site_coords(sites, out=c('site_name','lat','lon','alt'), use_basedon=TRUE, attach.units=FALSE)
  }
  if(calc_ts_needs$dvq_needs != '') {
    dvqcoefs <- get_meta('dvqcoefs')
  }
  needs_df <- as.data.frame(lapply(setNames(nm=needs_cols), function(need) {
    need_type <- strsplit(need, '\\.')[[1]][1]
    need_var <- strsplit(need, '\\.')[[1]][2]
    switch(
      need_type,
      var=ifelse(sites %in% list_sites(with_var_src=var_choices[[need]], logic='any'), 'true', NA),
      coord=ifelse(sites %in% coords[!is.na(coords[[need_var]]), 'site_name'], 'true', NA),
      dvqcoef=ifelse(sites %in% dvqcoefs[!is.na(dvqcoefs[[paste0('dvqcoefs.', need_var)]]), 'site_name'], 'true', NA)
    )
  }))
  
  # add the final no.data column (true if any dependency is missing) & return
  no_data <- mutate(needs_df, no.data=apply(needs_df, MARGIN=1, function(r) any(is.na(r))))
  return(no_data)
}
