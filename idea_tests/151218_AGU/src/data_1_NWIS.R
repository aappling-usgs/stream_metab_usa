# Figure: Map of pointdata

if((manual=FALSE)) {
  args <- list(outfile='out/data_1_NWIS.png')
} else {
  source("../../p1_import/code/process_make_args.R")
  args <- process_make_args(c("outfile"))
}

library(mda.streams)
library(unitted)
library(ggplot2)
library(ggmap)

datainfo <- v(get_meta(c('basic','metabinput')))

# states outlines in background
all_states <- map_data("state")
g <- ggplot() + geom_polygon(data=all_states, aes(x=long, y=lat, group=group), color="navy", fill='white')

# no borders, axis labels, etc.
g <- g + theme_classic() + 
  theme(axis.line = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())

# data, colored either by day of year or by length in days
g <- g + geom_point(data=filter(datainfo, !is.na(metabinput.num_dates)), aes(color=metabinput.num_dates, x=lon, y=lat, group=NA), size=4, alpha=1) +
  scale_colour_gradient('', low='darkorange', high='navy', guide=FALSE)
g
ggsave(args$outfile, plot=g, width=7.7, height=5)