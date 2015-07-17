source("p1_import/code/process_make_args.R")
args <- process_make_args(c("sb_user", "sb_password", "date", "tag", "strategy", "on_run_exists", "all_out_file", "on_ts_exists", "verbose"))

post_metab <- function(date="150717", tag="0.0.1", strategy="test write_metab_config", on_run_exists="addfiles", all_out_file=NA, on_ts_exists="skip", verbose=TRUE) {
  
  # identify folder that holds results
  out_dir <- paste0("p2_metab/out/", date, " ", tag, " ", strategy)
  if(!dir.exists(out_dir)) stop("couldn't find out_dir ", out_dir)
  if(verbose) message("output will be retrieved from '", out_dir, "'")
  
  # post the raw run files
  out_files <- dir(out_dir)
  post_metab_run(folder=out_dir, files=out_files, on_exists=on_run_exists)
  
  # post the metab_ts files if all_out_file is specified
  if(!is.na(all_out_file)) {
    load(file.path(out_dir, all_out_file)) # should create an all_out object in the environment
    if(!exists("all_out")) stop("expected all_out_file to contain an list named 'all_out' of metab_models")
    models_per_group <- 10
    models <- data.frame(
      model=1:length(all_out),
      group=rep(1:ceiling(length(all_out)/models_per_group), each=models_per_group)[1:length(all_out)], 
      stringsAsFactors=FALSE)
    
    for(g in unique(models$group)) {
      topost <- models[which(models$group==g),'model']
      message("posting metab results for all_out items ", paste0(topost, collapse=","))
      metab_ts_files <- stage_metab_ts(all_out[topost])
      post_ts(metab_ts_files, on_exists=on_ts_exists)
    }
  }  
  
  # report success
  
}
  