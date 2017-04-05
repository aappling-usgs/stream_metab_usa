#' Copy files into a single folder for transfer to HTCondor
#' 
#' set up your ~/.R/stream_metab.yaml with fields for the group's sb_user and 
#' sb_password first
#' 
#' @param cluster_dir the local folder whose contents you will manually transfer
#'   to the cluster after running this function
#' @param run.yaml list of config parameters, read from yaml and specific to
#'   this run, giving the desired model tag, posted_after, etc.
#' @param status.file the filename for the relevant files_metab.tsv
#' @param ... paths and names of other files to transfer
cluster_prep_condor <- function(cluster_dir='../2_metab_config/run1/cluster/condor', run.yaml, status.file, ...) {
  
  # create the destination directory if needed
  if(!dir.exists(cluster_dir)) {
    dir.create(cluster_dir, recursive=TRUE)
  }
  
  # identify the run ID to use for this run: use the first on the config list,
  # letting following IDs indicate previous runs
  runid <- run.yaml$runid[[1]]
  
  # collect arguments into list
  files <- unlist(list(...))
  
  # update the status file, save needed list for condor.sub updates below
  needed <- sb_check_model_status(status.file, run.yaml, cluster='condor')
  
  # also write needed to a new status file that's explicitly for jobs to be 
  # included in this cluster run. write it both to /out (with a runid stamp) and
  # to /condor (with no stamp)
  job.file <- file.path(dirname(status.file), paste0('cluster_jobs_', runid, '.tsv'))
  write.table(needed, job.file, row.names=FALSE, sep='\t')
  file.copy(job.file, file.path(cluster_dir, 'cluster_jobs.tsv'), overwrite=TRUE)
  
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
  }
  # map the log and results files to runid-specific folders
  condor.sub[grep('^output =', condor.sub)] <- sprintf('output = log_%s/$(Process).out', runid)
  condor.sub[grep('^error = ', condor.sub)] <- sprintf('error = log_%s/$(Process).err', runid)
  condor.sub[grep('^log = ', condor.sub)] <- sprintf('log = log_%s/$(Process).log', runid)
  condor.sub[grep('^transfer_output_remaps = ', condor.sub)] <- sprintf('transfer_output_remaps = "job = results_%s/job_$(Process)"', runid)
  # only request as many jobs as we need
  condor.sub[grep('^queue', condor.sub)] <- sprintf('queue %d', nrow(needed))
  # save the file
  writeLines(condor.sub, file.path(cluster_dir, 'condor.sub'))
  
  # remind how to run on cluster
  message(sprintf("1. scp or sftp the files in %s to the HTCondor submit node", cluster_dir))
  message("2. log on to the HTCondor submit node, then run these:")
  message("  dos2unix run_job.sh")
  message("  condor_submit condor.sub")
  message(sprintf("3. scp or sftp the files from results and log back to %s", file.path(dirname(dirname(cluster_dir)), 'out')))
  
}
