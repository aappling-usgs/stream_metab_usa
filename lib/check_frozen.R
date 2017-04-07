#' Checks a filename against a list of 'frozen' files, whose contents should not
#' be modified even if remake tries to call the file-generating function. This 
#' function can be added to any file-generating function whose products are 
#' likely to be frozen. Note that simply adding a filename to remake/freeze.yml 
#' won't work unless the file-generating function is also modified.
#' 
#' @param freezable the filename to check
#' @param freeze.file the name of the yaml file containing the list of frozen
#'   files
#' @examples 
#' myfun <- function(input, outfile) {
#'   if(check_frozen(outfile)) return(NULL)
#'   # other code to actually create outfile when unfrozen
#' }
check_frozen <- function(freezable, freeze.file='freeze.yml') {
  frozen <- unlist(yaml::yaml.load_file(freeze.file))
  if(freezable %in% frozen) {
    if(!file.exists(freezable)) {
      warning("file is frozen and doesn't exist: ", freezable)
    } else {
      warning("file is frozen; touching without update: ", freezable)
      system(paste("touch", freezable))
    }
    return(TRUE)
  } else {
    return(FALSE)
  }
}