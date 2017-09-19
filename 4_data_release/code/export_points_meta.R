#' Write out points_metadata to file so that 4_release_parent.yml needn't depend
#' on 4_release_spatial.yml (because that would cause a duplicate inclusion of
#' 1_site_data.yml via both files)
export_points_meta <- function(points_metadata, outfile) {
  writeLines(yaml::as.yaml(points_metadata), outfile)
}