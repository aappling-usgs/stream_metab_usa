#' Self-contained function to run the model and data specified by a specific
#' config row (job) and save the results in a specific output directory
#' 
#' 
run_model_job <- function(job, outdir, run_fun, retries=5, verbose=TRUE) {
  
  # load the basics
  library(dplyr, lib.loc='rLibs')
  library(tidyr, lib.loc='rLibs')
  library(ggplot2, lib.loc='rLibs')
  library(unitted, lib.loc='rLibs')
  library(streamMetabolizer, lib.loc='rLibs')
  library(mda.streams, lib.loc='rLibs')
  config <- read_config('config.tsv')
  status <- read.table('files_metab.tsv', header=TRUE, sep='\t', stringsAsFactors=FALSE)
  
  # plan to run the jobth row that's incomplete in status
  row_num <- filter(status, tagged==FALSE) %>%
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
    run_fun(config_row, verbose, outdir)
  }, 
  error=function(e) {
    warning(e)
    writeLines(e$message, file.path(outdir, sprintf("error %s.txt", stage_name)))
  })
  modeled <- any(substring(class(metab_out), 1, 5) == 'metab')
  if(!modeled) {
    message('modeling or summarization failed; see error file')
    return()
  }
  
  # stage/save
  if(verbose) message("saving metab_model for config row ", row_num, ": starting at ", Sys.time())
  stage_path <- stage_metab_model(
    title=stage_title, metab_outs=metab_out, folder=outdir, verbose=verbose)
  
  # post the output, with retries
  posted <- tagged <- FALSE
  for(attempt in seq_len(retries)) {
    if(!sbtools::is_logged_in()) login_sb()
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
  }
}
