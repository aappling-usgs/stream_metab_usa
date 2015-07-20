source("p1_import/code/process_make_args.R")
args <- process_make_args(c("sb_user", "sb_password", "tag", "strategy", "model", "model_args", "cluster", "verbose"))

model_metab <- function(tag="0.0.1", strategy="test write_metab_config", model='metab_mle', model_args='list()', cluster='local_process', verbose=TRUE) {
  
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
  
  # define model function
  run_config_to_metab <- function(row_id, sleep=runif(1, min=0, max=5)) {
    if(verbose) message("row ", row_id, ": starting at ", Sys.time())
    
    # really, truly model
    metab_out <- config_to_metab(config, row_id, verbose=TRUE)[[1]]
    
    # report
    out_file <- file.path(out_dir, sprintf("metab_out_%03d.RData", row_id))
    if(verbose && is.null(attr(metab_out, 'errors')) && !is.character(metab_out) && is(metab_out, 'metab_model')) {
      num_complete <- length(which(complete.cases(metab_out@fit[c('GPP','ER','K600')])))
      message("row ", row_id, ": metab_model produced with ", num_complete, " complete estimates")
      message("row ", row_id, ": saving to ", out_file)
    }
    
    # save the output. keep just a tiny bit of data, because saving it all would
    # be crazy - e.g., 1.7, 1.4 MB apiece, and redundant with what's already on
    # SB
    metab_out@data <- head(metab_out@data,10)
    save(metab_out, file=out_file)
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
    metab_all <- clusterApplyLB(c1, run_ids, run_config_to_metab)
    metab_all <- setNames(metab_all, run_names)
    stopCluster(c1)
  }
  
  # save everything
  all_out_file <- file.path(out_dir, "metab_all.RData")
  if(verbose) message("saving the full list of models to ", all_out_file)
  save(metab_all, file=all_out_file)
}
do.call(model_metab, args[c('tag','strategy','model','model_args','cluster','verbose')])