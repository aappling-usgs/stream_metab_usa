#' Return a vector of the sites for which final-round metabolism models were
#' attempted
get_modeled_sites <- function(configfile) {
  cfg <- read_config(configfile)
  unique(cfg$site)
}