# Figure: Regression of GPP day of peak versus a measure of stream size

if((manual=FALSE)) {
  args <- list(outfile='out/timing_7_peakvsize.png')
} else {
  source("../../p1_import/code/process_make_args.R")
  args <- process_make_args(c("outfile"))
}

source('src/timing_plots_lib.R')
load('cache/timing.RData')

g <- plot_timing_reg(
  mutate(timing, area_km2=landcover.ACCUM_AREA/1000000), 
  variables=c('doy_max_gpp','area_km2'), 
  short_labels=c(parse(text='GPP~peak'), parse(text='Watershed~area~(m^2)')), format_by='date') +
  scale_x_log10(breaks=c(1e1,1e3,1e5), labels=c('10','1000','100,000'))
ggsave(args$outfile, plot=g, width=3, height=3)
