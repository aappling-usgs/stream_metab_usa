

#' get time, lat, and lon grids for the chunk of data we need to pull out
#' 
#' @param ldas_config list config that specifies the data subset variables
#' @param target_name a dot-delimited output name (e.g., "nldas.grids")
nc_sub_grids <- function(ldas_config, target_name){
  
  # mock up huge request in order to get the nccopy response as an exception from GDP:
  data.name <- strsplit(target_name,'[.]')[[1]][1]
  if (data.name == 'nldas'){
    time.step <- 24 # hours per day
    # "13z01jan1979" is index 0
    
    time = c()
    time[1] <- (as.numeric(as.POSIXct(ldas_config$sub_times[1], tz='UTC')-as.POSIXct("1979-01-01 13:00 UTC", tz='UTC')))*time.step
    time[2] <- (as.numeric(as.POSIXct(ldas_config$sub_times[2], tz='UTC')-as.POSIXct("1979-01-01 13:00 UTC", tz='UTC')))*time.step
    
    lon <- c(0, 463) # this is known, we want the full dataset
    lat <- c(0, 223) # this is known, we want the full dataset
  } else if (data.name == 'gldas'){
    time.step <- 8 # steps per day
    time = c()
    time[1] <- (as.numeric(as.POSIXct(ldas_config$sub_times[1], tz='UTC')-as.POSIXct("2000-02-24 00:00 UTC", tz='UTC')))*time.step
    time[2] <- (as.numeric(as.POSIXct(ldas_config$sub_times[2], tz='UTC')-as.POSIXct("2000-02-24 00:00 UTC", tz='UTC')))*time.step
    lon <- c(50, 500) # this is a guess for something covering our project + AK and PR
    lat <- c(300, 599) # this is a guess for something covering our project + AK and PR
  } else {
    stop(data.name, ' is not supported')
  }
  
  
  return(data.frame(lon=lon,lat=lat,time=time, stringsAsFactors = FALSE))
}


#' create a file list for subsets of the netcdf data
#' 
#' @param grids data.frame of grids returned by nc_sub_grids
#' @param ldas_config list config that specifies the data subset variables
#' @param file.out the file to write the list of files to
ldas_sub_files <- function(grids, ldas_config, file.out){
  vars <- ldas_config$sub_variables
  start.i <- seq(as.numeric(grids$time[1]),to = as.numeric(grids$time[2]), by = ldas_config$sub_split)
  end.i <- c(tail(start.i-1,-1), as.numeric(grids$time[2]))
  data.name <- ldas_config$data_name
  
  # creates file string: "NLDAS_291000.291999_132.196_221.344_"
  time.files <- sprintf(paste0(sprintf("%s_%i.%i",data.name, start.i,end.i),'_%s.%s_%s.%s_'), grids$lat[1], grids$lat[2], grids$lon[1], grids$lon[2])
  file.names <- as.vector(unlist(sapply(time.files, paste0, vars,'.nc')))
  

  server.files <- ldas_server_files(ldas_config)
  new.files <- setdiff(file.names, server.files)
  rm.files <- setdiff(server.files, file.names)
  if (length(rm.files) > 0){
    message(sprintf('remove: %s',paste(paste0(rm.files), collapse=' ')))
    stop(sprintf('\n%s files are on the server but are no longer used and can be removed...',length(rm.files)))
  }
  # doing this explicitly to avoid the case where there are no new files, and we paste in a NA for the file name
  files <- ''
  for (file in new.files){
    files <- paste0(files, paste(file, ldas_config$ldas_url, sep='\t', collapse=''),'\n')
  }
  cat('file\turl\n', file=file.out, append = FALSE)
  cat(files, file=file.out, sep='', append = TRUE)
}

#' figure out what files already exist on the server
#' 
#' @param ldas_config list config that specifies the data subset variables
ldas_server_files <- function(ldas_config){
  server.data <- xmlParse(ldas_config$catalog_url, useInternalNodes = TRUE)
  
  nsDefs <- xmlNamespaceDefinitions(server.data )
  ns <- structure(sapply(nsDefs, function(x) x$uri), names = names(nsDefs))
  names(ns)[1] <- "xmlns"
  ncdf.datasets <- getNodeSet(server.data,'/xmlns:catalog/xmlns:dataset/xmlns:dataset[substring(@name, string-length(@name) - string-length(".nc") +1) = ".nc"]', ns)
  ncdf.files <- unlist(xmlApply(ncdf.datasets, function(x) xmlAttrs(x)[['name']]))
  ncdf.files <- ncdf.files[grepl(pattern = ldas_config$data_name, ncdf.files)] # match to dataset name
  if (is.null(ncdf.files))
    ncdf.files = c() # no files on the server
  return(ncdf.files)
}

#' actually move the files from one server to another
#' 
#' @param file.list a file that contains a list of the files to get and move to another server
#' @param mssg.file the status file to log details to
#' @param internal.confg a list that contains some things that shouldn't be checked into a repo, including server dir structure
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
    
    url <- sprintf('http%s?lon%s,time%s,lat%s,%s%s%s%s', substr(data.url, 5, stop = nchar(data.url)), lon.i, time.i, lat.i, var, time.i, lat.i, lon.i)
    
    # to tempfolder...
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

#' create an ncml file for thredds that describes the dataset
#' 
#' @param server.files a vector of file names that are on the thredds server
#' @param ncml.out a file name for the ncml file to be created
create_ldas_ncml <- function(server.files, ncml.out, ldas.config){
  files <- server.files
  
  times <- sort(unique(unname(sapply(files,function(x) paste(strsplit(x, '[_]')[[1]][1:4], collapse='_')))))
  vars <- unique(unname(sapply(files,function(x) paste(strsplit(tail(strsplit(x, '[_]')[[1]],1),'[.]')[[1]][1], collapse='_'))))
  
  ncml <- newXMLNode('netcdf', namespace=c("http://www.unidata.ucar.edu/namespaces/netcdf/ncml-2.2", xlink="http://www.w3.org/1999/xlink"))
  if (!missing(ldas.config)){
    for (var in names(ldas.config$units)){
      var.details <- newXMLNode('variable', parent = ncml, attrs = c(name=var))
      newXMLNode('attribute', parent = var.details, attrs = c(name="units", value=ldas.config$units[[var]] ))
    }
  }
  agg <- newXMLNode('aggregation', parent = ncml, attrs = c(type="union"))
  newXMLNode('remove', parent = agg, attrs = c(type="attribute", name="Conventions"))
  for (var in vars){
    nc <- newXMLNode('netcdf', parent = agg)
    join <- newXMLNode('aggregation', parent = nc, attrs=c(type="joinExisting", dimName="time"))
    for (t in times){
      newXMLNode('netcdf', parent = join, attrs=c(location=sprintf("%s_%s.nc", t, var)))
    }
    
  }
  saveXML(ncml, file = ncml.out)
}

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


