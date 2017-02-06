# Self-contained function to run the model and data specified by a specific
# config row (job) and save the results in a specific output directory
run_model <- function(job, outdir, retries=5, verbose=TRUE) {
  
  # load the basics
  library(streamMetabolizer)
  library(mda.streams)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(unitted)
  config <- read_config('config.tsv')
  status <- read.table('status.tsv')
  
  # use status to figure out which row to run
  row_num <- filter(status, tagged=FALSE)[job, 'config_row']
  
  # model metabolism
  if(verbose) message("modeling metabolism for config row ", row_num, ": starting at ", Sys.time())
  metab_out <- config_to_metab(config=config, rows=row_num, verbose=verbose)[[1]]
  
  # stage/save
  if(verbose) message("saving metab_model for config row ", row_num, ": starting at ", Sys.time())
  stage_title <- make_metab_run_title(
    format(as.Date(config[row_num,'date']), '%y%m%d'), config[row_num,'tag'], config[row_num,'strategy'])
  staged <- stage_metab_model(
    title=stage_title, 
    metab_outs=metab_out, 
    folder=outdir, 
    verbose=verbose)
  stage_name <- make_metab_model_name(stage_title, row_num, config[row_num,'site'])
  
  # post the output, with retries
  for(attempt in seq_len(retries)) {
    complete <- tryCatch({
      if(verbose) message("posting metab_model for config row ", row_num, ": starting at ", Sys.time())
      if(!sbtools::is_logged_in()) sbtools::authenticate_sb(sb_user, sb_password)
      post_metab_model(staged)
      TRUE
    }, 
    error=function(e) {
      message('error in post_metab_model: ', e)
      message(paste0('sleeping for ', attempt^2,' minutes before retry'))
      Sys.sleep(30*attempt^2) # sleep a good long while to let SB recover
      repair_metab_model(stage_name, verbose=verbose) # would be nice to do this only if the error is specific to a tagging problem
      Sys.sleep(30*attempt^2) # and a good while longer
      FALSE
    })
    # decide whether to return or retry
    if(isTRUE(complete)) {
      break
    }
  }
  
  # clean up before returning if posting worked; otherwise leave the model in
  # place to be zipped and returned
  if(isTRUE(complete)) {
    file.remove(staged)
    message('model was successfully run and posted')
    invisible()
  }
}
