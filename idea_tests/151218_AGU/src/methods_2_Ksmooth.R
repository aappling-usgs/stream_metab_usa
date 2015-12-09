# Figure: K predictions and observations

if((manual=FALSE)) {
  args <- list(outfile='out/methods_2_Ksmooth.png')
} else {
  source("../../p1_import/code/process_make_args.R")
  args <- process_make_args(c("outfile"))
}

library(ggplot2)
library(dplyr)

load('cache/preds_2_K.RData')
g <- preds_2_K %>% filter(site=='nwis_02203700', !is.na(K600)) %>%
  ggplot(aes(x=velocity.daily)) +
  geom_point(aes(y=K600.obs, shape='A', linetype='A', color='A')) +
  geom_line(aes(y=K600, shape='B', linetype='B', color='B'), size=1) + 
  scale_color_manual('', values=c(A='navy',B='violetred1'), labels=c('Phase 1', 'Phase 2&3')) + 
  scale_shape_manual('', breaks=factor(c('A', 'B')), values=c(A=19, B=NA), labels=c(A='Phase 1', B='Phase 2&3')) + 
  scale_linetype_manual('', breaks=factor(c('A', 'B')), values=c(A=0, B=1), labels=c(A='Phase 1', B='Phase 2&3')) +
  scale_x_log10(breaks=c(0.1,0.2,0.4)) + 
  scale_y_log10(labels=scales::format_format(scientific=FALSE, drop0trailing=TRUE)) +
  theme_classic() + ylab(parse(text="K[600]~(d^-1)")) + xlab(parse(text="Velocity~(m~s^-1)"))
ggsave(filename=args$outfile, plot=g, width=6, height=3)
