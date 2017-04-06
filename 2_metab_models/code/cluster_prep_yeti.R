#' Copy files into a single folder for transfer to HTCondor
#' 
#' set up your ~/.R/stream_metab.yaml with fields for the group's sb_user and 
#' sb_password first
#' 
#' @param cluster_dir the local folder whose contents you will manually transfer
#' @param run.yaml list of config parameters, read from yaml and specific to 
#'   this run, giving the desired model tag, posted_after, etc.
#' @param ... paths and names of other files to transfer
cluster_prep_yeti <- function(cluster_dir='../2_metab_models/run1/cluster/yeti', run.yaml, ...) {
  
  # create the destination directory if needed
  if(!dir.exists(cluster_dir)) {
    dir.create(cluster_dir, recursive=TRUE)
  }
  
  # collect arguments into list
  files <- unlist(list(...))
  
  # update the status file, save needed list for condor.sub updates below
  status.file <- grep('files_metab\\.tsv', files, value=TRUE)
  needed <- sb_check_model_status(status.file, run.yaml, cluster='yeti')
  
  # copy files into the condor directory
  for(file in files) {
    success <- file.copy(file, file.path(cluster_dir, basename(file)), overwrite=TRUE)
    if(!success) stop("file could not be copied: ", file)
  }
  
  # create directories to accept output files
  dirs <- c('log','results')
  for(dir in dirs) {
    login_node_dir <- file.path(cluster_dir, dir)
    if(!dir.exists(login_node_dir)) {
      dir.create(login_node_dir)
    }
  }
  
  # modify the condor submit file for this run
  condor.sub <- readLines(file.path(cluster_dir, 'condor.sub'))
  if(grepl('run1', cluster_dir)) {
    condor.sub[grep('request_cpus', condor.sub)] <- 'request_cpus = 1'
  }
  condor.sub[grep('queue', condor.sub)] <- sprintf('queue %d', 1) #### TEMPORARY ####
  #condor.sub[grep('queue', condor.sub)] <- sprintf('queue %d', nrow(needed))
  writeLines(condor.sub, file.path(cluster_dir, 'condor.sub'))
  
  # remind how to run on cluster
  message(sprintf("1. scp or sftp the files in %s to the Yeti submit node", cluster_dir))
  message("2. log on to the Yeti submit node, then run these:")
  message("R")
  message("  # unzip the package bundle")
  message("  unzip('bundle.zip')")
  message("  # ensure there's a directory to install packages locally")
  message("  libdir <- '/cxfs/projects/usgs/water/owi/powstreams/rLibs'")
  message("  if(!dir.exists(libdir)) dir.create(libdir)")
  message("  # install the packages needed for this job")
  message("  source('install_packages.R')")
  message("  install_packages(libdir, oldonly=TRUE)")
  message("  q()")
  message('sbatch array.batch')
  message(sprintf("3. scp or sftp the files from results and log back to %s", file.path(dirname(dirname(cluster_dir)), 'out')))
  
}
