#' Copy files into a single folder for transfer to HTCondor
#' 
#' set up your ~/.R/stream_metab.yaml with fields for the group's sb_user and
#' sb_password first
#' 
#' @param cluster_dir the local folder whose contents you will manually transfer
cluster_prep_condor <- function(cluster_dir='../2_metab_config/prep/cluster/condor', ...) {
  
  files <- unlist(list(...))
    for(file in files) {
    success <- file.copy(file, file.path(cluster_dir, basename(file)), overwrite=TRUE)
    if(!success) stop("file could not be copied: ", file)
  }
  
  dirs <- c('out','log','err','results')
  for(dir in dirs) {
    login_node_dir <- file.path(cluster_dir, dir)
    if(!dir.exists(login_node_dir)) {
      dir.create(login_node_dir)
    }
  }
  
}
