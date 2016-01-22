# Figure: Map of sites by duration of season

if((manual=FALSE)) {
  args <- list(outfile='out/timing_2_durhist.png')
} else {
  source("../../p1_import/code/process_make_args.R")
  args <- process_make_args(c("outfile"))
}

source('src/timing_plots_lib.R')
load('cache/timing.RData')

g <- plot_timing_hist(timing, 'fwhm', 'Season length (days)', format_by='difftime')
ggsave(args$outfile, plot=g, width=3, height=3)
