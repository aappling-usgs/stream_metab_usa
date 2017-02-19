#' Copy files into a single folder for transfer to HTCondor
#' 
#' set up your ~/.R/stream_metab.yaml with fields for the group's sb_user and
#' sb_password first
#' 
#' @param cluster_dir the local folder whose contents you will manually transfer
cluster_prep_condor <- function(cluster_dir='../2_metab_config/prep/cluster/condor', smu.config, status.file, ...) {
  
  # identify the run ID to use for this run: use the first on the config list,
  # letting following IDs indicate previous runs
  runid <- if(grepl('/prep/', cluster_dir)) smu.config$prep_runid[[1]] else smu.config$runid[[1]]
  
  # collect arguments into list
  files <- unlist(list(...))
  
  # update the status file, save needed list for condor.sub updates below
  needed <- sb_check_model_status(status.file, smu.config, cluster='condor')
  
  # also write needed to a new status file that's explicitly for jobs to be 
  # included in this cluster run. write it both to /out (with a runid stamp) and
  # to /condor (with no stamp)
  job.file <- file.path(dirname(status.file), paste0('cluster_jobs_', runid, '.tsv'))
  write.table(needed, job.file, row.names=FALSE, sep='\t')
  file.copy(job.file, file.path(cluster_dir, 'cluster_jobs.tsv'))
  
  # copy files into the condor directory
  for(file in files) {
    success <- file.copy(file, file.path(cluster_dir, basename(file)), overwrite=TRUE)
    if(!success) stop("file could not be copied: ", file)
  }
  
  # create directories to accept output files
  dirs <- paste0(c('log_','results_'), runid)
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
