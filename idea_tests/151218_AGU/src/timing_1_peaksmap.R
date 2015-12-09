# Figure: Map of sites by timing of annual peak

if((manual=FALSE)) {
  args <- list(outfile='out/timing_1_peaksmap.png')
} else {
  source("../../p1_import/code/process_make_args.R")
  args <- process_make_args(c("outfile"))
}

library(dplyr)
library(ggmap)
library(ggplot2)

load('cache/siteyears_clean.RData')

source('src/timing_lib.R')
sydat <- describe_siteyears(siteyears_clean)
sygdat <- describe_siteyears(sitegrows_clean)
# fill in doy_max_gpp where it's NA in sydat but complete in sygdat
sydat[!sydat$complete & sygdat$complete,'doy_max_gpp'] <- sygdat$doy_max_gpp[!sydat$complete & sygdat$complete]
sydat[!sydat$complete & sygdat$complete,'doy_max_er'] <- sygdat$doy_max_er[!sydat$complete & sygdat$complete]
sydat[!sydat$complete & sygdat$complete,'complete'] <- TRUE
# simplify to one row per site
sydatmeans <- sydat %>% filter(complete) %>% group_by(site) %>% 
  summarize(
    years=n(), lat=lat[1], lon=lon[1],
    doy_max_gpp=mean(doy_max_gpp)) %>%
  mutate(month_max_gpp=ordered(format(as.Date("2014-12-31") + as.difftime(doy_max_gpp, units='days'), "%b"),
                               c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')))

date_breaks <- as.numeric(format(as.Date(c("2015-03-21","2015-06-21","2015-09-21","2015-12-21")), "%j"))
date_labels <- format(as.Date("2014-12-31") + as.difftime(date_breaks, units='days'), "%b %d")
all_states <- map_data("state")
g <- ggplot() + geom_polygon(data=all_states, aes(x=long, y=lat, group=group), color="lightgrey", fill='white') +
  theme_classic() + xlab('Longitude') + ylab('Latitude') +
  geom_point(data=sydatmeans, aes(x=lon, y=lat, group=NA, color=doy_max_gpp), size=4, alpha=1) +
  scale_colour_gradientn('Mean Date of\nPeak GPP', colours = rainbow(7), breaks=date_breaks, labels=date_labels) +
  theme(axis.line = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank(), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank())
ggsave(args$outfile, plot=g, width=9, height=5)

# could try mapping by the day on which half of the year's productivity has occurred

# ggplot() + geom_polygon(data=all_states, aes(x=long, y=lat, group=group), color="lightgrey", fill='white') +
#   theme_classic() + xlab('Longitude') + ylab('Latitude') +
#   geom_point(data=sydatmeans, aes(x=lon, y=lat, group=NA, color=month_max_gpp), size=5, alpha=1)
