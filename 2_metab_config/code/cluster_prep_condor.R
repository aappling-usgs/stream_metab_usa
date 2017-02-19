#' Copy files into a single folder for transfer to HTCondor
#' 
#' set up your ~/.R/stream_metab.yaml with fields for the group's sb_user and
#' sb_password first
#' 
#' @param cluster_dir the local folder whose contents you will manually transfer
cluster_prep_condor <- function(cluster_dir='../2_metab_config/prep/cluster/condor', smu.config, ...) {
  
  # collect arguments into list
  files <- unlist(list(...))
  
  # update the status file, save needed list for condor.sub updates below
  status.file <- grep('files_metab\\.tsv', files, value=TRUE)
  needed <- sb_check_model_status(status.file, smu.config, cluster='condor')
  
  # copy files into the condor directory
  for(file in files) {
    success <- file.copy(file, file.path(cluster_dir, basename(file)), overwrite=TRUE)
    if(!success) stop("file could not be copied: ", file)
  }
  
  # create directories to accept output files
  dirs <- c('log_170218','results_170218')
  for(dir in dirs) {
    login_node_dir <- file.path(cluster_dir, dir)
    if(!dir.exists(login_node_dir)) {
      dir.create(login_node_dir)
    }
  }
  
  # modify the condor submit file for this run
  condor.sub <- readLines(file.path(cluster_dir, 'condor.sub'))
  if(grepl('prep', cluster_dir)) {
    condor.sub[grep('request_cpus', condor.sub)] <- 'request_cpus = 1'
  } else {
    # any mods for the main run?
  }
  #condor.sub[grep('queue', condor.sub)] <- sprintf('queue %d', 1) #### TEMPORARY ####
  condor.sub[grep('queue', condor.sub)] <- sprintf('queue %d', nrow(needed))
  writeLines(condor.sub, file.path(cluster_dir, 'condor.sub'))
  
  # remind how to run on cluster
  message(sprintf("1. scp or sftp the files in %s to the HTCondor submit node", cluster_dir))
  message("2. log on to the HTCondor submit node, then run these:")
  message("  dos2unix run_job.sh")
  message("  condor_submit condor.sub")
  message(sprintf("3. scp or sftp the files from results and log back to %s", file.path(dirname(dirname(cluster_dir)), 'out')))
  
}
