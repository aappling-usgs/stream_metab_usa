#' load the user's internal (private, hidden, local) profile information from a 
#' file
#' 
#' @param filename filename to use for internal profile yaml. Defaults to 
#'   $HOME/.R/stream_metab.yaml if missing.
#' @import yaml
#' @seealso create_default_profile
load_profile <- function(filename=file.path(Sys.getenv("HOME"), ".R", "stream_metab.yaml")) {
  if(!file.exists(filename)) {
    create_default_profile(filename)
  }
  return(yaml::yaml.load_file(filename))
}

#' create a default internal profile yaml
create_default_profile <- function(filename) {
  message("creating default internal profile file at ", filename)
  configDir <- dirname(filename)
  if (!file.exists(configDir)) {
    dir.create(configDir, recursive = TRUE)
  }
  writeLines(c("sb_user: ''", "sb_password: ''"), con=filename)
}
