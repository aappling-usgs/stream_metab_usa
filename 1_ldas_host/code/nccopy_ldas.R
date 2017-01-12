#' actually move the files from one server to another
#' 
#' @param file.list a file that contains a list of the files to get and move to 
#'   another server
#' @param mssg.file the status file to log details to
#' @param internal.confg a list that contains some things that shouldn't be 
#'   checked into a repo, including server dir structure. The fields used are 
#'   \code{metab_user} and \code{thredds_dir}. See 
#'   \code{\link[mda.streams]{login_sb}} for basic setup requirements for the 
#'   profile; these fields may be added.
nccopy_ldas <- function(file.list, mssg.file, internal.config){
  files <- read.table(file.list, sep='\t', stringsAsFactors = FALSE, header = TRUE)
  
  cat('index of new files contains', length(files$file), file=mssg.file, append = FALSE)
  if (length(files$file) == 0){
    message('no new files to nccopy, doing nothing')
    return()
  }
  data.url <- unique(files$url)
  stopifnot(length(data.url) == 1)
  write_grid <- function(x){
    v <- strsplit(x,'[.]')[[1]]
    sprintf("[%s:1:%s]",v[1],v[2])
  }
  
  registerDoMC(cores=4)
  foreach(file=files$file) %dopar% {
    local.nc.file <- file.path(tempdir(), file)
    
    
    file.chunks <- strsplit(file,'[_]')[[1]]
    lat.i = write_grid(file.chunks[3])
    lon.i = write_grid(file.chunks[4])
    time.i = write_grid(file.chunks[2])
    var = strsplit(file.chunks[5],'[.]')[[1]][1]
    
    url <- sprintf('https%s?lon%s,time%s,lat%s,%s%s%s%s', substr(data.url, 5, stop = nchar(data.url)), lon.i, time.i, lat.i, var, time.i, lat.i, lon.i)
    
    # to tempfolder...
    # instructions from: https://disc.gsfc.nasa.gov/registration/registration-for-data-access#ncdump
    # and followed the ncdump additional file creation, including making ~/.dodsrc with cookie locations. This worked for nccopy
    output <- system(sprintf("nccopy -m 15m %s %s", url, local.nc.file))
    cat(sprintf('\n** nccopy %s%s to %s...', var, time.i, basename(local.nc.file)), file=mssg.file, append = TRUE)
    if (!output){
      cat('done! **', file=mssg.file, append = TRUE)
      
      output <- system(sprintf('rsync -rP --rsync-path="sudo -u tomcat rsync" %s %s@cida-eros-netcdfdev.er.usgs.gov:%s%s', local.nc.file, internal.config$metab_user, internal.config$thredds_dir, file),
                       ignore.stdout = TRUE, ignore.stderr = TRUE)
      cat('\n** transferring file to thredds server...', file=mssg.file, append = TRUE)
      #rsync, and verify that is good
      if (!output){
        cat('done! **', file=mssg.file, append = TRUE)
        message('rsync of ',file, ' complete! ', Sys.time())
      } else {
        cat(url, ' FAILED **', file=mssg.file, append = TRUE)
      }
      
    } else {
      cat(url, ' FAILED **', file=mssg.file, append = TRUE)
    }
    unlink(local.nc.file)
    
  }
  
}
