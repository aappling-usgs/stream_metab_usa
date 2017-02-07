#' Copy files into a single folder for transfer to HTCondor
#' 
#' set up your ~/.R/stream_metab.yaml with fields for the group's sb_user and
#' sb_password first
#' 
#' @param cluster_dir the local folder whose contents you will manually transfer
cluster_prep_condor <- function(cluster_dir='../2_metab_config/prep/cluster/condor', smu.config, ...) {
  
  # copy files into the condor directory
  files <- unlist(list(...))
    for(file in files) {
    success <- file.copy(file, file.path(cluster_dir, basename(file)), overwrite=TRUE)
    if(!success) stop("file could not be copied: ", file)
  }
  
  # create directories to accept output files
  dirs <- c('out','log','err','results')
  for(dir in dirs) {
    login_node_dir <- file.path(cluster_dir, dir)
    if(!dir.exists(login_node_dir)) {
      dir.create(login_node_dir)
    }
  }
  
  # modify the condor submit file for this run
  condor.sub <- readLines(file.path(cluster_dir, 'condor.sub'))
  if(grepl('prep', cluster_dir)) {
    condor.sub <- condor.sub %>%
      gsub('request_cpus = 4', 'request_cpus = 1', .)
  } else {
    # any mods for the main run?
  }
  needed <- sb_check_model_status(file.path(cluster_dir, 'files_metab.tsv'), phase=c('stage','post'), smu.config)
  needed <- data.frame(x=1:2) #### FOR TESTING ONLY ####
  condor.sub <- condor.sub %>%
    gsub('queue 2', sprintf('queue %d', nrow(needed)), .)
  writeLines(condor.sub, file.path(cluster_dir, 'condor.sub'))
  
  # remind how to run on cluster
  message(sprintf("1. scp or sftp the files in %s to the HTCondor submit node", cluster_dir))
  message("2. log on to the HTCondor submit node, then run these:")
  message("  dos2unix run_job.sh")
  message("  condor_submit condor.sub")
  message(sprintf("3. scp or sftp the files back to %s", cluster_dir))
  
}
