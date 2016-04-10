#' load the internals from a file
#' 
#' @param filename filename to use for internal yaml. Defaults to a yaml in R HOME if missing.
load_internal = function(filename) {
  if (missing(filename)) {
    filename <- file.path(Sys.getenv("HOME"), ".R", "stream_metab.yaml")
  }
  
  return(yaml::yaml.load_file(filename))
}

#' uses a yaml file to login to sciencebase
#' 
#' 
auth_internal = function(){
  internals <- load_internal()
  authenticate_sb(internals$sb_user, internals$sb_password)
}