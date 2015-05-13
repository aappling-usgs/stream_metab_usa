# Download existing timeseries data from ScienceBase.
# Ted, this is your file to modify as you like.

# If needed: 
# library(devtools)
# install_github("USGS-R/powstreams")
# install_github("USGS-R/streamMetabolizer")
# install.packages("dataRetrieval", repos=c("http://usgs-r.github.com","http://cran.us.r-project.org"), dependencies=TRUE, type="both")

library(powstreams) # This is our main data-accessing package for the Powell Center project
?powstreams # See a list of functions this way
library(streamMetabolizer) # This is the metabolism modeling package
library(dataRetrieval) # This is a more general data-accessing package for NWIS data
library(dplyr) # This is a generally awesome package. Gets you %>%, mutate, select, group_by, summarize, ...

# Get a list of IDs & info for streams on ScienceBase
metab_variables <- c("disch","doobs","wtr")
sb_site_ids <- powstreams::list_sites(with_timeseries=metab_variables)
sb_site_ids <- setNames(sb_site_ids, sb_site_ids)
sb_site_names <- sapply(strsplit(sb_site_ids, split = '_'), tail, 1) %>%
  dataRetrieval::readNWISsite() %>% 
  mutate(site_id=paste0("nwis_", site_no)) %>% 
  select(site_id, station_nm, dec_lat_va, dec_long_va)

# Download the site data - these should be stored on your computer but never
# added to the GitHub repository
get_data_again <- FALSE
if(get_data_again == TRUE) {
  site_data <- lapply(sb_site_ids, function(site_id) {
    one_site <- load_timeseries(site=site_id, variables=metab_variables, join.fun=inner_join)
    write.csv(one_site, paste0("sandbox_ted/data/", site_id, ".csv"), row.names=FALSE)
  })
}  

# Read the site data from csv back into R, if you like. Now you have a list of
# data.frames.
site_data <- lapply(sb_site_ids, function(site_id) {
  read.csv(paste0("sandbox_ted/data/", site_id, ".csv"), header=TRUE)
})

# You could inspect one at a time
head(site_data[[3]])

# Or you could loop through to compute a metric, such as mean amplitude
mean_DO_amp <- sapply(sb_site_ids[1:7], function(site_id) {
  one_site <- site_data[[site_id]] %>%
    mutate(
      DateTime = streamMetabolizer::convert_GMT_to_solartime(
        as.POSIXct(DateTime, tz="GMT"), 
        longitude=powstreams::site_location(site_id)$longitude, time.type="apparent solar"),
      Date=as.Date(DateTime)) %>%
    group_by(Date) %>%
    summarize(DO_Amp = max(ts_doobs, na.rm=TRUE) - min(ts_doobs, na.rm=TRUE))
  mean(one_site$DO_Amp)
})