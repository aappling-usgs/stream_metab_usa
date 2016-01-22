# Figure: Regression of DOY of ER peak vs GPP peak

if((manual=FALSE)) {
  args <- list(outfile='out/timing_6_peakEvG.png')
} else {
  source("../../p1_import/code/process_make_args.R")
  args <- process_make_args(c("outfile"))
}

source('src/timing_plots_lib.R')
load('cache/timing.RData')

g <- plot_timing_EvG(timing, 'doy_max', 'peak', format_by='date')
ggsave(args$outfile, plot=g, width=3, height=3)
