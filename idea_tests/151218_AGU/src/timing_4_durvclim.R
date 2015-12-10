# Figure: Regression of GPP season length versus a measure of climate

if((manual=FALSE)) {
  args <- list(outfile='out/timing_4_durvclim.png')
} else {
  source("../../p1_import/code/process_make_args.R")
  args <- process_make_args(c("outfile"))
}

source('src/timing_plots_lib.R')
load('cache/timing.RData')

g <- plot_timing_reg(
  mutate(timing, nofreeze=climate.FstFz_RE-climate.LstFz_RE), 
  variables=c('fwhm_gpp','nofreeze'), 
  short_labels=c('GPP season length (days)', 'Frost-free season length (days)'), format_by='difftime') +
  geom_abline(aes(color='1:1 line'), linetype='dashed', show_guide=TRUE) + 
  scale_color_manual('', breaks='1:1 line', values='navy') +
  theme(legend.position=c(0.9,0.15))
ggsave(args$outfile, plot=g, width=3, height=3)
