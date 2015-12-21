# Figure: Map of example NLDAS data

if((manual=FALSE)) {
  args <- list(outfile='out/data_2_NLDAS.png')
} else {
  source("../../p1_import/code/process_make_args.R")
  args <- process_make_args(c("outfile"))
}

library(dplyr)
library(mda.streams)
library(ggplot2)
library(ggmap)
library(rasterVis)
library(grid)

load('cache/nldas.RData')

# get states outlines
all_states <- map_data("state")
usa_outline <- map_data('usa')
NW_point <- which.min(usa_outline$long)
NE_point <- which.max(usa_outline$long)
usa_inverse <- bind_rows(
  bind_rows(
    data.frame(long=nldas@extent@xmax, lat=c(nldas@extent@ymax, usa_outline$lat[NE_point]), group=NA, order=NA, region='', subregion=NA),
    usa_outline[4237:6095,],
    data.frame(long=nldas@extent@xmin, lat=c(usa_outline$lat[NW_point], nldas@extent@ymax), group=NA, order=NA, region='', subregion=NA)) %>%
    mutate(group=1, order=1:n(), region='north', subregion=NA),
  bind_rows(
    data.frame(long=nldas@extent@xmin, lat=c(nldas@extent@ymin, usa_outline$lat[NE_point]), group=NA, order=NA, region='', subregion=NA),
    usa_outline[c(6096:6886,1:4237),],
    data.frame(long=nldas@extent@xmax, lat=c(usa_outline$lat[NW_point], nldas@extent@ymin), group=NA, order=NA, region='', subregion=NA)) %>%
    mutate(group=2, order=1:n(), region='south', subregion=NA)
)

# plot
g <- gplot(nldas, maxpixels = 5e5) + 
  geom_tile(aes(fill = value), alpha=1) +
  geom_polygon(data=usa_inverse, aes(x=long, y=lat, group=group), color="transparent", fill='white') +
  geom_polygon(data=all_states, aes(x=long, y=lat, group=group), color="navy", fill='transparent') +
  scale_fill_gradient('Light (W/m^2)', low='lightgray', high='orange', guide=FALSE) +
  #coord_equal() + 
  theme_classic() + 
  theme(axis.line = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())
ggsave(args$outfile, plot=g, width=7.5, height=5)
