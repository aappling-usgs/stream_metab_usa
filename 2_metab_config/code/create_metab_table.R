#' Create a file to record metab model run/stage/post status
create_metab_table <- function(config, outfile) {
  # Only create and update a fresh table if no current one exists. This fixes 
  # the problem where the table was getting created here, updated in stage_ts, 
  # and then overwritten by a re-creation because remake notices that the file 
  # is now different from its hashed file. And stage_ts and sb_post_ts do plenty
  # of status checking themselves, so no need even to check the status here if
  # the file already exists.
  if(!file.exists(outfile)) {
    run_title <- make_metab_run_title(
      format(as.Date(config[,'date']), '%y%m%d'), config[,'tag'], config[,'strategy'])
    model_names <- make_metab_model_name(run_title, 1:nrow(config), config[,'site'])
    model_files <- make_metab_model_path(model_name, '.')
    
    # create and write the model status table
    metab.table <- data.frame(filename=model_files, local=FALSE, error=FALSE, posted=FALSE, tagged=FALSE)
    write_status_table(site.table, outfile)
  }
  
  # udpate the local/posted/tagged columns
  sb_check_model_status(outfile, phase='stage')
  sb_check_model_status(outfile, phase='post', posted_after=config$posted_after)
}