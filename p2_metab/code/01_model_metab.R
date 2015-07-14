source("p1_metab/code/process_make_args.R")
args <- process_make_args(c("sb_user", "sb_password", "model", "model_args", "verbose"))

model_metab <- function(tag="0.0.1", strategy="test write_metab_config", model='metab_mle', model_args='list()', verbose=TRUE) {
  
  tag="0.0.1"
  strategy="first full run"
  # identify folder to hold results
  out_dir <- paste0("p2_metab/out/", format(Sys.Date(), "%y%m%d"), " ", tag, " ", strategy)
  if(!dir.exists(out_dir)) dir.create(out_dir)
  config_path <- file.path(out_dir, "condor_config.tsv")
  
  # stage
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
  
  out <- config_to_metab(config_path, rows=10:12)
}
