#' get time, lat, and lon grids for the chunk of data we need to pull out
#' 
#' @param ldas_config list config that specifies the data subset variables
#' @param target_name a dot-delimited output name (e.g., "nldas.grids")
nc_sub_grids <- function(ldas_config, target_name){
  
  data.name <- toupper(strsplit(target_name,'[.]')[[1]][1])
  if (data.name == 'NLDAS'){
    time.step <- 24 # hours per day
    # "13z01jan1979" is index 0
    
    time = c()
    time[1] <- (as.numeric(as.POSIXct(ldas_config$sub_times[1], tz='UTC')-as.POSIXct("1979-01-01 13:00 UTC", tz='UTC')))*time.step
    time[2] <- (as.numeric(as.POSIXct(ldas_config$sub_times[2], tz='UTC')-as.POSIXct("1979-01-01 13:00 UTC", tz='UTC')))*time.step
    
    lon <- c(0, 463) # this is known, we want the full dataset
    lat <- c(0, 223) # this is known, we want the full dataset
  } else if (data.name == 'GLDAS'){
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
