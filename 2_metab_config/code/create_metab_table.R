#' Create a file to record metab model run/stage/post status
#' 
#' @section columns:
#' \itemize{
#'  \item{"filepath"}{Local path where model file should be staged before 
#'  posting. This is unchanged in the stage and post phases. Also used by 
#'  \code{sb_check_model_status()} to determine the file name where the model
#'  should be on ScienceBase.}
#'  \item{"local"}{Starts as FALSE. If it can be confirmed in
#'  \code{sb_check_model_status()} that the file has been successfully
#'  downloaded (by \code{run_model()}) to the local temp directory, this is set
#'  to TRUE in the row for that file.}
#'  \item{"remote"}{Starts as FALSE. If it can be confirmed in
#'  \code{sb_check_model_status()} that the file has been successfully posted to
#'  ScienceBase, this is set to TRUE in the corresponding row.}
#'  \item{"error"}{Starts as NA. If it is discovered in
#'  \code{sb_check_model_status()} that an error has occurred in running,
#'  staging, or posting the model, a string giving the error will be entered in
#'  the corresponding row.}
#' }
#' 
#' @import mda.streams
#' @seealso write_status_table sb_check_model_status
create_metab_table <- function(config_file="../2_metab_config/prep/out/config.tsv", smu.config) {
  
  # convert the config_file into the config table and an output file location. I
  # wish remake would let us specify the outfile as a separate argument, but
  # then it would be a file dependency before it's been created.
  config <- read_config(config_file)
  outfile <- file.path(dirname(config_file), "files_metab.tsv")
  
  # Only create and update a fresh table if no current one exists. This fixes 
  # the problem where the table was getting created here, updated in an outside
  # call to sb_check_model_status, and then overwritten by a re-creation because
  # remake notices that the file is now different from its hashed file.
  if(!file.exists(outfile)) {
    run_title <- make_metab_run_title(
      format(as.Date(config[,'date']), '%y%m%d'), config[,'tag'], config[,'strategy'])
    model_names <- make_metab_model_name(run_title, 1:nrow(config), config[,'site'])
    model_files <- make_metab_model_path(model_names, '.')
    
    # create and write the model status table
    metab.table <- data.frame(filepath=model_files, local=FALSE, posted=FALSE, tagged=FALSE, error=as.character(NA))
    write_status_table(metab.table, outfile)
  }
  
  # update the local/posted/tagged columns
  sb_check_model_status(outfile, phase='stage')
  posted_after <- if(grepl('prep', config_file)) smu.config$prep_posted_after else smu.config$posted_after
  sb_check_model_status(outfile, phase='post', posted_after=posted_after)
}