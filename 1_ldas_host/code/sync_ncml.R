#' move the ncml file out to the server
#' 
#' @param file the ncml file (see create_ldas_ncml)
#' @param internal.confg a list that contains some things that shouldn't be checked into a repo, including server dir structure
sync_ncml <- function(file, internal.config){
  server.file <- basename(file)
  output <- system(sprintf('rsync -rP --rsync-path="sudo -u tomcat rsync" %s %s@cida-eros-netcdfdev.er.usgs.gov:%s%s', file,  internal.config$metab_user, internal.config$thredds_dir, server.file),
                   ignore.stdout = TRUE, ignore.stderr = TRUE)
  if (!output)
    invisible(output)
  else 
    stop(output)
}
