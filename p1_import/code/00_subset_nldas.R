

nc_sub_grids <- function(nldas_config){
  # mock up huge request in order to get the nccopy response as an exception from GDP:
  
  stencil <- webgeom('state')
  fabric <- webdata(url=nldas_config$nldas_url, variables=nldas_config$sub_variables, times=nldas_config$sub_times)
  knife <- webprocess('subset', OUTPUT_TYPE="netcdf", REQUIRE_FULL_COVERAGE = FALSE)
  job <- geoknife(stencil, fabric, knife = knife, wait=TRUE)
  grid.data <- strsplit(check(job)$status,'[,?]')[[1]][-1]
  get_grid <- function(data){
    strsplit(strsplit(strsplit(data,'[[]')[[1]][2],'[]]')[[1]],'[:]')[[1]][-2]
  }
  
  lon <- c(0, 463) # this is known, we want the full dataset
  lat <- c(0, 223)
  time <- get_grid(grid.data[2]) # this is unknown, and based on `times`
  
  return(data.frame(lon=lon,lat=lat,time=time, stringsAsFactors = FALSE))
}

nldas_sub_files <- function(grids, nldas_config, server.files, file.out){
  vars <- nldas_config$sub_variables
  start.i <- seq(as.numeric(grids$time[1]),to = as.numeric(grids$time[2]), by = nldas_config$sub_split)
  end.i <- c(tail(start.i-1,-1), as.numeric(grids$time[2]))
  
  # creates file string: "NLDAS_291000.291999_132.196_221.344_"
  time.files <- sprintf(paste0(sprintf("NLDAS_%i.%i",start.i,end.i),'_%s.%s_%s.%s_'), grids$lat[1], grids$lat[2], grids$lon[1], grids$lon[2])
  file.names <- as.vector(unlist(sapply(time.files, paste0, vars,'.nc')))
  
  new.files <- setdiff(file.names, server.files)
  rm.files <- setdiff(server.files, file.names)
  if (length(rm.files) > 0){
    message(sprintf('remove: %s',paste(paste0(rm.files), collapse=' ')))
    stop(sprintf('\n%s files are on the server but are no longer used and can be removed...',length(rm.files)))
  }
  
  files <- paste(new.files, nldas_config$nldas_url, sep='\t', collapse='\n')
  
  cat('file\turl\n', file=file.out, append = FALSE)
  cat(files, '\n', file=file.out, sep='', append = TRUE)
}

nldas_server_files <- function(nldas_config){
  server.data <- xmlParse(nldas_config$catalog_url, useInternalNodes = TRUE)
  
  nsDefs <- xmlNamespaceDefinitions(server.data )
  ns <- structure(sapply(nsDefs, function(x) x$uri), names = names(nsDefs))
  names(ns)[1] <- "xmlns"
  ncdf.datasets <- getNodeSet(server.data,'/xmlns:catalog/xmlns:dataset/xmlns:dataset[substring(@name, string-length(@name) - string-length(".nc") +1) = ".nc"]', ns)
  ncdf.files <- unlist(xmlApply(ncdf.datasets, function(x) xmlAttrs(x)[['name']]))
  if (is.null(ncdf.files))
    ncdf.files = c() # no files on the server
  return(ncdf.files)
}

nccopy_nldas <- function(file.list, mssg.file, internal.config){
  
  files <- read.table(file.list, sep='\t', stringsAsFactors = FALSE, header = TRUE)
  
  cat('index of new files contains', length(files$file), file=mssg.file, append = FALSE)
  
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
    
    url <- sprintf('http%s?lon%s,time%s,lat%s,%s%s%s%s', substr(data.url, 5, stop = nchar(data.url)), lon.i, time.i, lat.i, var, time.i, lat.i, lon.i)
    
    # to tempfolder...
    output <- system(sprintf("nccopy -m 15m %s %s", url, local.nc.file))
    cat(sprintf('\n** nccopy %s%s to %s...', var, time.i, local.nc.file), file=mssg.file, append = TRUE)
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

load_internal = function(filename) {
  if (missing(filename)) {
    filename <- file.path(Sys.getenv("HOME"), ".R", "stream_metab.yaml")
  }
  
  return(yaml::yaml.load_file(filename))
}
