#' Self-contained function to run the model and data specified by a specific 
#' config row (job) and save the results in a specific output directory. When
#' this is called, the requisite packages should already be installed and
#' argument values filled out.
run_model_job <- function(job, outdir, run_fun, retries=5, verbose=TRUE) {
  
  # load the basics
  library(methods)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(unitted)
  library(streamMetabolizer)
  library(mda.streams)
  config <- read_config('config.tsv')
  status <- read.table('cluster_jobs.tsv', header=TRUE, sep='\t', stringsAsFactors=FALSE)
  
  # plan to run the jobth row of the status file
  row_num <- status %>%
    .[[job, 'filepath']] %>%
    parse_metab_model_path(out='row') %>%
    as.numeric()
  config_row <- filter(config, config.row == row_num)
  
  # get standardized names for the model for interacting with SB
  stage_title <- make_metab_run_title(
    format(as.Date(config_row$date), '%y%m%d'), config_row$tag, config_row$strategy)
  stage_name <- make_metab_model_name(
    stage_title, config_row$config.row, config_row$site)
  
  # model metabolism
  metab_out <- tryCatch({
    if(verbose) message("modeling metabolism for config row ", row_num, ": starting at ", Sys.time())
    run_fun(config_row, verbose, outdir, stage_name)
  },
  error=function(e) {
    message('modeling or summarization had an error; see error file')
    writeLines(e$message, file.path(outdir, sprintf("error %s.txt", stage_name)))
    warning(e$message)
  })
  modeled <- any(substring(class(metab_out), 1, 5) == 'metab')
  if(!modeled) {
    message("modeling or summarization output wasn't complete; see partial file")
    message(paste0("class(metab_out): ", class(metab_out)))
    stop(metab_out)
  }
  
  # stage/save
  if(verbose) message("saving metab_model for config row ", row_num, ": starting at ", Sys.time())
  stage_path <- stage_metab_model(
    title=stage_title, metab_outs=metab_out, folder=outdir, verbose=verbose)
  
  # post the output, with retries
  posted <- tagged <- FALSE
  for(attempt in seq_len(retries)) {
    if(!sbtools::is_logged_in()) login_sb(filename='stream_metab.yaml')
    # try to post
    if(!posted) {
      posted <- tryCatch({
        if(verbose) message("posting metab_model for config row ", row_num, ": starting at ", Sys.time())
        model_id <- post_metab_model(stage_path, on_exists='skip')
        length(model_id) == 1 && !is.list(model_id) && !is.na(model_id) # i forget which format a bad post has
      }, 
      error=function(e) {
        message('error in post_metab_model: ', e$message)
        FALSE
      })
      Sys.sleep(20) # always sleep a little before checking
      tagged <- !is.na(locate_metab_model(stage_name, by="tag"))
    }
    
    # try to repair
    if(posted && !tagged) {
      repair_metab_model(stage_name, verbose=verbose)
      Sys.sleep(20) # always sleep a little before checking
      tagged <- !is.na(locate_metab_model(stage_name, by="tag"))
    }
    
    # either return or sleep & retry
    if(posted && tagged) {
      break
    } else {
      message(paste0('sleeping for ', attempt^2,' minutes before retry'))
      Sys.sleep(60*attempt^2) # sleep a good long while to let SB recover
    }
  }
  
  # clean up before returning if posting worked; otherwise leave the model in
  # place to be zipped and returned
  if(posted && tagged) {
    file.remove(stage_path)
    message('model was successfully run and posted')
    invisible()
  } else {
    message("posting failed; saved the model .RData file")
  }
}
