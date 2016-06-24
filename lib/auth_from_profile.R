#' uses a yaml file to login to sciencebase
#' 
#' @param filename the profile filename where the username and password are
#'   located
auth_from_profile = function(filename=file.path(Sys.getenv("HOME"), ".R", "stream_metab.yaml")){
  profile <- load_profile(filename)
  authenticate_sb(profiles$sb_user, profiles$sb_password)
}