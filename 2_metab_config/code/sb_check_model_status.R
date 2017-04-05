#' Update the metab.file based on what's staged locally, and return metab model 
#' filenames for any models that aren't yet staged (or posted; see \code{phase})
#' 
#' @param metab.file the filename of the status table
#' @param run.yaml the read-in values for this run from metab_configs_config.yml
#' @param cluster NA to do nothing, or the name of an element from 
#'   metab_clusters_config.yml to filter to only models in that element's list
#' @param clust.config.file the filename of the cluster configuration file
#' @seealso read_status_table write_status_table
#' @import dplyr
#' @import mda.streams
#' @import sbtools
sb_check_model_status <- function(metab.file, run.yaml, cluster=NA, clust.config.file='../2_metab_config/in/metab_clusters_config.yml') {
  
  posted_after <- as.POSIXct(run.yaml$posted_after)
  
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
  mms_by_dir <- unlist(lapply(mms_query[mms_posted_after], function(mmitem) mmitem$title))
  
  # get mm tags. many SB calls =(
  mms_by_tag <- unlist(lapply(mms_query[mms_posted_after], function(mmitem) sbtools::item_get_fields(mmitem, 'identifiers')[[1]]$key))
  
  # find status of metab.table elements by comparing above lists
  metab.table$posted <- mms_by_tbl %in% mms_by_dir
  metab.table$tagged <- mms_by_tbl %in% mms_by_tag
  
  # assign clusters according to their presence in the config
  assignments <- yaml::yaml.load_file(clust.config.file)
  metab.table$assigned_to <- as.character(NA)
  for(i in 1:length(assignments)) {
    rows <- sapply(assignments[[i]], grep, metab.table$filepath)
    if(!is.list(rows)) {
      clust.name <- names(assignments)[i]
      metab.table$assigned_to[rows] <- clust.name
    }
  }
  
  # write the revised site status table
  write_status_table(metab.table, metab.file)  
  
  # determine and return the list of models that still need to be staged/posted/reposted
  needed <- filter(metab.table, !posted | !tagged)
  if(!is.na(cluster)) needed <- filter(needed, is.na(assigned_to) | assigned_to==cluster)
  needed
}

