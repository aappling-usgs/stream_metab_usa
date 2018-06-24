#' @param spatial_points data frame of a superset of model site points, with
#'   columns for site_name, lat, lon, and coord_datum
#' @param model_config_tsv tsv of the final model config file from which the
#'   final site ID list should be drawn
prepare_model_points_metadata <- function(spatial.sites, model_config_tsv) {
  # read the model config file to learn which sites were modeled in the end
  model_config <- readr::read_tsv(model_config_tsv)
  model_site_ids <- unique(model_config$site)
  
  # filter the site points (a regular data frame with lat and lon columns) to just the modeled sites
  model.spatial.sites <- spatial.sites[spatial.sites$site_name %in% model_site_ids, ]
  
  # convert the filtered site points to a spatial points data frame
  model.spatial.points <- create_site_points(model.spatial.sites)
  
  # create and return a metadata list with the spatial data (bbox, states, etc.)
  # for use in building complete metadata files
  meddle::extract_feature(model.spatial.points)
}