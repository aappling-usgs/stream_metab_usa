#' load the user's internal (private, hidden, local) profile information from a 
#' file
#' 
#' @param filename filename to use for internal yaml. Defaults to 
#'   $HOME/.R/stream_metab.yaml if missing.
load_internal = function(filename=file.path(Sys.getenv("HOME"), ".R", "stream_metab.yaml")) {
  if(!file.exists(filename)) {
    create_default_internal(filename)
  }
  return(yaml::yaml.load_file(filename))
}

#' create a default internal profile yaml
create_default_internal <- function(filename) {
  message("creating default internal profile file at ", filename)
  configDir <- dirname(filename)
  if (!file.exists(configDir)) {
    dir.create(configDir, recursive = TRUE)
  }
  writeLines(c("sb_user: ''", "sb_password: ''"), con=filename)
}

#' uses a yaml file to login to sciencebase
#' 
#' 
auth_internal = function(filename=file.path(Sys.getenv("HOME"), ".R", "stream_metab.yaml")){
  internals <- load_internal(filename)
  authenticate_sb(internals$sb_user, internals$sb_password)
}