# Figure: Histogram of timing of season peak

if((manual=FALSE)) {
  args <- list(outfile='out/timing_5_peakhist.png')
} else {
  source("../../p1_import/code/process_make_args.R")
  args <- process_make_args(c("outfile"))
}

source('src/timing_plots_lib.R')
load('cache/timing.RData')

g <- plot_timing_hist(timing, 'doy_max', 'Day of annual peak', format_by='date')
ggsave(args$outfile, plot=g, width=3, height=3)
