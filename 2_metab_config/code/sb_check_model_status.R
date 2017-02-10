#' Update the metab.file based on what's staged locally, and return metab model
#' filenames for any models that aren't yet staged (or posted; see \code{phase})
#' 
#' @param metab.file the filename of the status table
#' @param phase should the table be updated for the staging phase or the posting
#'   phase?
#' @seealso read_status_table write_status_table
#' @import dplyr
#' @import mda.streams
#' @import sbtools
sb_check_model_status <- function(metab.file, smu.config) {
  posted_after <- as.POSIXct(if(grepl('prep', metab.file)) smu.config$prep_posted_after else smu.config$posted_after)
  
  # read in the status table
  metab.table <- read_status_table(metab.file)
  details <- parse_metab_model_path(metab.table$filepath, use_names=FALSE)
  tag <- unique(details$tag)
  
  # recalculate the 'remote' column
  message('checking metab model status on ScienceBase')
  login_sb()
  
  # note what's available locally
  relpaths <- file.path(dirname(dirname(metab.file)), 'out', metab.table$filepath)
  metab.table$local <- ifelse(is.na(metab.table$local), NA, file.exists(relpaths))
  
  # get mms listed in metab.table
  mms_by_tbl <- details$model_name
  
  # get mms by query from SB on text; filter by posted_after
  mms_query <- sbtools::query_item_in_folder(text=tag, folder=locate_folder('metab_models'), limit=10000) 
  mms_posted_after <- sapply(mms_query, function(mmd) max(sapply(mmd$files, function(f) f$dateUploaded))) > as.POSIXct(posted_after, tz='UTC')
  mms_by_dir <-  unlist(lapply(mms_query[mms_posted_after], function(mmitem) sbtools::item_get_fields(mmitem, 'identifiers')[[1]]$key))
  
  # get mm tags. many SB calls =(
  mms_by_tag <- unlist(lapply(mms_query, function(mmitem) sbtools::item_get_fields(mmitem, 'identifiers')[[1]]$key))
  
  # find status of metab.table elements by comparing above lists
  metab.table$posted <- mms_by_tbl %in% mms_by_dir
  metab.table$tagged <- mms_by_tbl %in% mms_by_tag
  
  # determine what's still needed
  needed <- filter(metab.table, !posted | !tagged)
  
  # write the revised site status table
  write_status_table(metab.table, metab.file)  
  
  # return the list of models that still need to be staged/posted/reposted
  needed
}

