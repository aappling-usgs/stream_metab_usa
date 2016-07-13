#' create a file list for subsets of the netcdf data
#' 
#' @param grids data.frame of grids returned by nc_sub_grids
#' @param ldas_config list config that specifies the data subset variables
#' @param file.out the file to write the list of files to
ldas_sub_files <- function(grids, ldas_config, file.out){
  data.name <- toupper(strsplit(file.out,'[/_]')[[1]][6])
  vars <- ldas_config[[data.name]]$sub_variables
  start.i <- seq(as.numeric(grids$time[1]),to = as.numeric(grids$time[2]), by = ldas_config[[data.name]]$sub_split)
  end.i <- c(tail(start.i-1,-1), as.numeric(grids$time[2]))
  
  # creates file string: "NLDAS_291000.291999_132.196_221.344_"
  time.files <- sprintf(paste0(sprintf("%s_%i.%i",data.name, start.i,end.i),'_%s.%s_%s.%s_'), grids$lat[1], grids$lat[2], grids$lon[1], grids$lon[2])
  file.names <- as.vector(unlist(sapply(time.files, paste0, vars,'.nc')))
  
  
  server.files <- ldas_server_files(ldas_config, data.name)
  new.files <- setdiff(file.names, server.files)
  rm.files <- setdiff(server.files, file.names)
  if (length(rm.files) > 0){
    message(sprintf('remove: %s',paste(paste0(rm.files), collapse=' ')))
    stop(sprintf('\n%s files are on the server but are no longer used and can be removed...',length(rm.files)))
  }
  # doing this explicitly to avoid the case where there are no new files, and we paste in a NA for the file name
  files <- ''
  for (file in new.files){
    files <- paste0(files, paste(file, ldas_config[[data.name]]$ldas_url, sep='\t', collapse=''),'\n')
  }
  cat('file\turl\n', file=file.out, append = FALSE)
  cat(files, file=file.out, sep='', append = TRUE)
}
