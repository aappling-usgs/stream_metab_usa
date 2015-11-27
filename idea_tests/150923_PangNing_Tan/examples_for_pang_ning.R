library(powstreams)
library(ggplot2)
library(unitted)

sites <- get_meta(c("basic","manual"))
sites <- sites[sites$manual.assessment %in% c("accept","review"),]

i=1
inf=NA
ts=NA
site=NA

plot_next_site <- function(v="ER", s) {
  if(missing(s)) {
    site <<- sites[i,"site_name"]; i <<- i+1; cat(paste0(i-1, ") ", site))
  } else {
    site <<- s
  }
  print(inf <<- get_site_info(site))
  if(v == "GPP"){
    ts <<- get_ts("gpp_estBest",site)
    print(summarize_ts("gpp_estBest",site))
    print(ggplot(v(ts), aes(x=DateTime, y=gpp)) + geom_point() + ggtitle(paste(site, inf$long_name)))
  } else if(v=="ER") {
    ts <<- get_ts("er_estBest",site)
    print(summarize_ts("er_estBest",site))
    print(ggplot(v(ts), aes(x=DateTime, y=er)) + geom_point() + ggtitle(paste(site, inf$long_name)))
  } else {
    stop("uh oh")
  }
}

ggplot(v(ts), aes(x=DateTime, y=gpp)) + geom_point() + ggtitle(paste(site, inf$long_name)) + ylim(-5,15) + theme_bw()
ggplot(v(ts), aes(x=DateTime, y=er)) + geom_point() + ggtitle(paste(site, inf$long_name)) + ylim(-20,5) + theme_bw()

#' nwis_01400500 Raritan River at Manville NJ - single summertime bulge
#' nwis_01463500 Delaware River at Trenton NJ - several peaks per summer, interannual variation
#' nwis_01480617 West Branch Brandywine Creek at Modena, PA - winter gaps. peak in early spring, declines through rest of summer
#' nwis_01481500 BRANDYWINE CREEK AT WILMINGTON, DE - beautiful 1 peak per year
#' 
#' nwis_01481500
#' nwis_01542500
#' nwis_01493112
#' nwis_01649500
#' nwis_01650800
#' nwis_02035000
#' nwis_02110400
#' nwis_02156500
#' nwis_02160700