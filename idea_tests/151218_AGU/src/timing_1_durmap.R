# Figure: Map of sites by timing of season duration

if((manual=FALSE)) {
  args <- list(outfile='out/timing_1_durmap.png')
} else {
  source("../../p1_import/code/process_make_args.R")
  args <- process_make_args(c("outfile"))
}

source('src/timing_plots_lib.R')
load('cache/timing.RData')

g <- plot_timing_map(timing, 'fwhm_gpp', 'GPP season length\n(full width 1/5th\nmaximum, days)', format_by='difftime')
ggsave(args$outfile, plot=g, width=9, height=5)
