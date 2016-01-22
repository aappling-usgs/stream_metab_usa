# Figure: Regression of ER season duration vs GPP season duration

if((manual=FALSE)) {
  args <- list(outfile='out/timing_3_durEvG.png')
} else {
  source("../../p1_import/code/process_make_args.R")
  args <- process_make_args(c("outfile"))
}

source('src/timing_plots_lib.R')
load('cache/timing.RData')

g <- plot_timing_EvG(timing, 'fwhm', 'season length (days)', format_by='difftime')
ggsave(args$outfile, plot=g, width=3, height=3)
