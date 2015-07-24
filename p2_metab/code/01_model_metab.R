source("p1_import/code/process_make_args.R")
args <- process_make_args(c("sb_user", "sb_password", "tag", "strategy", "model", "model_args", "cluster", "verbose"))

model_metab <- function(tag="0.0.4", strategy="nighttime_k", model='metab_night', model_args='list()', post=FALSE, cluster='local_process', verbose=TRUE) {

  # identify folder to hold results
  out_dir <- paste0("p2_metab/out/", format(Sys.Date(), "%y%m%d"), " ", tag, " ", strategy)
  if(!dir.exists(out_dir)) dir.create(out_dir)
  config_path <- file.path(out_dir, "config.tsv")
  if(verbose) message("output will be saved to: ", out_dir)
  
  # stage & load config
  if(!file.exists(config_path)) {
    if(verbose) message("generating config_file")
    sites <- list_sites(list(
      "sitetime_calcLon", 
      "doobs_nwis", 
      any=c("dosat_calcGGbts","dosat_calcGGbconst"),
      "depth_calcDisch",
      "wtr_nwis",
      any=c("par_nwis","par_calcLat")), 
      logic="all")
    config_file <- stage_metab_config(
      tag=tag, strategy=strategy, 
      model=model, model_args=model_args,
      site=sites, filename=config_path)
  } else {
    if(verbose) message("using existing config_file; to replace change the tag, strategy, or file")
    config_file <- config_path
  }
  
  # define model function. variables the caller needs to know: verbose, config, out_dir, cluster
  run_config_to_metab <- function(row_id, sleep=runif(1, min=0, max=5)) {
    if(verbose) message("row ", row_id, ": starting at ", Sys.time())
    
    # really, truly model
    metab_out <- config_to_metab(config, row_id, verbose=TRUE)[[1]]
    
    # report
    if(verbose && is.null(attr(metab_out, 'errors')) && !is.character(metab_out) && is(metab_out, 'metab_model')) {
      if(config[row_id,'model']=='metab_night') {
        num_complete <- length(which(complete.cases(metab_out@fit[c('ER','K600')])))
      } else {
        num_complete <- length(which(complete.cases(metab_out@fit[c('GPP','ER','K600')])))
      }
      message("row ", row_id, ": metab_model produced with ", num_complete, " complete estimates")
    }
    
    # save the output. keep just a tiny bit of data, because saving it all would
    # be crazy - e.g., 1.7, 1.4 MB apiece, and redundant with what's already on
    # SB
    metab_out@data <- rbind(head(metab_out@data,5), tail(metab_out@data,5))
    if(cluster != 'condor_cluster') {
      out_file <- file.path(out_dir, sprintf("metab_out_%03d.RData", row_id))
      message("row ", row_id, ": saving to ", out_file)
      save(metab_out, file=out_file)
    }
    
    metab_out
  }
  
  # execute model function
  config <- read.table(config_path, sep="\t", header=TRUE, colClasses="character")
  run_ids <- 1:nrow(config)
  if(verbose) message("running metabolism models on a ", cluster, " for ", length(run_ids)," of ", nrow(config), " config rows")
  run_names <- paste0("run_", run_ids)
  if(cluster=='local_process') {
    all_out <- lapply(setNames(run_ids, run_names), run_config_to_metab, sleep=0)
  } else if(cluster=='local_cluster') {
    library(parallel)
    # My machine has '8' nodes, which probably means 4 hyperthreaded nodes, so 6
    # may be a maximal use of those resources.
    c1 <- makePSOCKcluster(rep('localhost', 6), outfile='')
    clusterCall(c1, function() { library(mda.streams) })
    assign('config', envir=.GlobalEnv, value=config) # clusterExport looks in .GlobalEnv
    clusterExport(c1, 'config')
    all_out <- clusterApplyLB(c1, run_ids, run_config_to_metab)
    all_out <- setNames(all_out, run_names)
    stopCluster(c1)
  } else if(cluster=='condor_cluster') {
    all_out <- model_metab_by_condor_cluster() # lengthy, so defined below
  }
  
  # save everything
  all_out_file <- file.path(out_dir, "metab_all.RData")
  if(verbose) message("saving the full list of models to ", all_out_file)
  save(all_out, file=all_out_file)
}

model_metab_by_condor_cluster <- function() {
  
  #### Launch ####
  
  # Start a cluster, wait for them to connect
  c1 = makePSOCKcluster(paste0('machine', 1:50), manual=TRUE, port=4043)
  #' At this point go to putty, cd into condor_R_snow.
  #' 
  #' 3. Edit simple.sh to contain MASTER=YOUR.LOCAL.IP.ADDRESS (can find from
  #' R with system("ipconfig"))
  #' 
  #' 4. Type condor_submit condor.sub. This will yield the text:
  #' 
  #' Submitting
  #' job(s).......................................................... 60
  #' job(s) submitted to cluster XXXX.
  #' 
  #' 5. Query for updates using condor_q. We're looking for jobs to appear in
  #' that list with the cluster ID declared above (whatever number replaces
  #' XXXX).
  #' 
  #' 6. We're also looking to see them show up in the local console as replies
  #' to the makePSOCKcluster call. If the local console appears to have
  #' stalled while condor_q suggests all jobs have been dispatched, you can
  #' run condor_submit condor.sub again to add more worker nodes. Any nodes
  #' that don't find a master just disappear.
  #' 
  #' 7. When the makePSOCKcluster call has finished, we're ready to make 
  #' clusterCalls.

    
  #### Configure ####
  
  # Define utility fun and send it to all nodes
  install_check <- function(repo, pkg, ghuser) {
    # Install
    run_install <- function() {
      capture.output(
        switch(repo,
               'cran'={ install.packages(pkg, repos='http://cran.rstudio.com') },
               'gran'={ install.packages(pkg, repos='http://owi.usgs.gov/R') },
               'github'={ install_github(paste0(ghuser, "/", pkg)) }))
    }
    out <- tryCatch(
      { run_install() }, 
      error=function(e) { list(output=NA, warnings=NA, errors=e) }, 
      warning=function(w) { list(output=run_install(), warnings=w, errors=NA) })
    
    # Report whether it installed
    itworked <- (pkg %in% unname(installed.packages()[,'Package']))
    return(list(itworked, out))
  }
  clusterExport(c1, 'install_check')
  
  # Define local utilities for looking at results
  all_first_out <- function(out) {
    all(sapply(out, function(o) o[[1]]))
  }
  view_install_out <- function(out) {
    lapply(out, function(o) { cat(o[[2]], '\n') })
    invisible()
  }
  
  pkg_needs <- c('dataRetrieval', 'devtools', 'dplyr', 'geoknife', 'httr', 'jsonlite', 
                 'LakeMetabolizer', 'lazyeval', 'lubridate', 'mda.streams', 'methods', 'parallel', 'RCurl', 
                 'runjags', 'sbtools', 'streamMetabolizer', 'stringr', 'unitted', 'XML') #'rjags', 
  
  view_installed_packages <- function() {
    inst <- clusterCall(c1, function() { unname(installed.packages()[,"Package"]) })
    pkg_needs=pkg_needs
    suppressWarnings(
      as.data.frame(inst) %>%
        setNames(paste0('n',1:length(c1))) %>%
        gather(key=node, value=pkg, 1:length(c1)) %>%
        filter(pkg %in% pkg_needs) %>%
        mutate(has=TRUE) %>%
        full_join(data.frame(pkg=pkg_needs, node=factor("none"), has=FALSE), by=c("node","pkg","has")) %>%
        spread(key=node, value=has) %>%
        select(-none))
  }
  
  # Now actually install packages, checking for completion each time. already installed: methods, parallel
  all_first_out(out <- clusterCall(c1, function(){ install_check('cran', 'devtools') }))
  all_first_out(out <- clusterCall(c1, function(){ install_check('cran', 'dplyr') })) # also installs lazyeval
  all_first_out(out <- clusterCall(c1, function(){ install_check('cran', 'httr') })) # also installs jsonlite, stringr
  all_first_out(out <- clusterCall(c1, function(){ install_check('cran', 'LakeMetabolizer') }))
  all_first_out(out <- clusterCall(c1, function(){ install_check('cran', 'lubridate') }))
  all_first_out(out <- clusterCall(c1, function(){ install_check('cran', 'RCurl') }))
  all_first_out(out <- clusterCall(c1, function(){ install_check('cran', 'reshape2') }))
  #all_first_out(out <- clusterCall(c1, function(){ install_check('cran', 'rjags') }))
  all_first_out(out <- clusterCall(c1, function(){ install_check('cran', 'runjags') }))
  all_first_out(out <- clusterCall(c1, function(){ install_check('cran', 'XML') }))
  all_first_out(out <- clusterCall(c1, function(){ install_check('gran', 'dataRetrieval') }))
  clusterCall(c1, function() { library(devtools) })
  all_first_out(out <- clusterCall(c1, function(){ install_check('github', 'unitted', 'appling') }))
  all_first_out(out <- clusterCall(c1, function(){ install_check('github', 'geoknife', 'USGS-R') }))
  all_first_out(out <- clusterCall(c1, function(){ install_check('github', 'sbtools', 'aappling-usgs') }))
  all_first_out(out <- clusterCall(c1, function(){ install_check('github', 'streamMetabolizer', 'aappling-usgs') }))
  all_first_out(out <- clusterCall(c1, function(){ install_check('github', 'mda.streams', 'aappling-usgs') }))
  
  # DIAGNOSTICS
  # for diagnosing the single most recent install run:
  out
  view_install_out(out)
  # there should be a JAGS folder listed here for every node
  clusterCall(c1, function() { dir("/usr/local/lib") })
  
  # for seeing overall install status
  view_installed_packages()#[,1:6]
  
  #### Do Science ####
  
  # load packages
  clusterCall(c1, function() { library(mda.streams) })
  
  # transfer variables used by run_config_to_metab. clusterExport looks in
  # .GlobalEnv, so put them there first
  assign('config', envir=.GlobalEnv, value=config)
  assign('verbose', envir=.GlobalEnv, value=verbose)
  assign('out_dir', envir=.GlobalEnv, value=out_dir)
  assign('cluster', envir=.GlobalEnv, value=cluster)
  clusterExport(c1, 'config')
  clusterExport(c1, 'verbose')
  clusterExport(c1, 'out_dir')
  clusterExport(c1, 'cluster')
  
  # run the model
  all_out <- clusterApplyLB(c1, run_ids, run_config_to_metab)
  all_out <- setNames(all_out, run_names)
  

  ### CAREFUL! ### clean up the cluster if we're done
  stopCluster(c1)
  
  # Return
  all_out
}


#### Run the makefile function defined above ####
do.call(model_metab, args[c('tag','strategy','model','model_args','cluster','verbose')])