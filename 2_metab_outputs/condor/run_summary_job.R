#' Self-contained function to run the model and data specified by a specific 
#' config row (job) and save the results in a specific output directory. When
#' this is called, the requisite packages should already be installed and
#' argument values filled out.
run_summary_job <- function(rows, outdir='job') {
  
  # load the basics
  library(methods)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(unitted)
  library(streamMetabolizer)
  library(mda.streams)
  source('make_model_summary.R')
  source('make_model_tses.R')
  source('make_model_preds.R')
  source('make_model_fit.R')
  
  # plan to run the jobth row of the status file
  config <- read_config('config.tsv') %>%
    mutate(
      resolution = substring(strategy, 7),
      model_name = make_metab_model_name(title=make_metab_run_title(date=format(as.Date(date), '%y%m%d'), tag=tag, strategy=strategy), row=config.row, site=site)) %>%
    filter(config.row %in% rows)
  
  # create directories for outputs
  sumdir <- file.path(outdir, 'summaries')
  tsdir <- file.path(outdir, 'tses')
  predsdir <- file.path(outdir, 'preds')
  fitdir <- file.path(outdir, 'fits')
  lapply(c(sumdir, tsdir, predsdir, fitdir), function(xdir) if(!dir.exists(xdir)) dir.create(xdir))
  
  # create the individual files for each model (config.row)
  login_sb()
  for(cr in seq_len(nrow(config))) {
    message("summarizing config row ", config$config.row[cr])
    mm <- get_metab_model(config$model_name[cr], version='original', update_sb=FALSE)
    make_model_summary(mm, outdir=sumdir) # for devs + data paper
    make_model_tses(mm, outdir=tsdir) # for collaborators
    make_model_preds(mm, outdir=predsdir) # for data paper
    make_model_fit(mm, outdir=fitdir) # ofr data paper
    rm(mm)
    gc()
  }
  
}
