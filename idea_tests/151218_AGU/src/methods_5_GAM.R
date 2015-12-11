# Figure: Smoothed predictions

if((manual=FALSE)) {
  args <- list(outfile='out/methods_5_GAM.png')
} else {
  source("../../p1_import/code/process_make_args.R")
  args <- process_make_args(c("outfile"))
}

library(dplyr)
library(ggplot2)

load('cache/clean_2_GAM.RData')

plot_gam <- function(s='nwis_385446094430700', v='ER') {
  scale_breaks <- c('Phase 3','GAM smooth')
  scale_labs <- scale_breaks %>% setNames(scale_breaks)
  scale_cols <- c('orange', 'navy') %>% setNames(scale_breaks)
  scale_pch <- c(19, NA) %>% setNames(scale_breaks)
  scale_lty <- c(0, 1) %>% setNames(scale_breaks)
  
  dat <- clean_2_GAM %>% filter(site==s)
  dat[[paste0(v,'.keep')]] <- ifelse(dat$all.keep, dat[[v]], NA)
  g <- ggplot(dat, aes(x=local.date, group=gam.chunk))
  switch(
    v,
    GPP={
      g <- g +
        geom_point(aes(y=GPP.keep, color='Phase 3', shape='Phase 3', linetype='Phase 3')) + 
        geom_ribbon(aes(ymin=GPP.gam.lower, ymax=GPP.gam.upper), fill='navy', alpha=0.2) + 
        geom_line(aes(y=GPP.gam, color='GAM smooth', shape='GAM smooth', linetype='GAM smooth'))
    },
    ER={
      g <- g +
        geom_point(aes(y=ER.keep, color='Phase 3', shape='Phase 3', linetype='Phase 3')) + 
        geom_ribbon(aes(ymin=ER.gam.lower, ymax=ER.gam.upper), fill='navy', alpha=0.2) + 
        geom_line(aes(y=ER.gam, color='GAM smooth', shape='GAM smooth', linetype='GAM smooth'))
    },
    K600={
      g <- g +
        geom_point(aes(y=K600.keep, color='Phase 3', shape='Phase 3', linetype='Phase 3')) + 
        geom_ribbon(aes(ymin=K600.gam.lower, ymax=K600.gam.upper), fill='navy', alpha=0.2) + 
        geom_line(aes(y=K600.gam, color='GAM smooth', shape='GAM smooth', linetype='GAM smooth'))
    }
  )
  g +
    scale_color_manual('', breaks=scale_breaks, values=scale_cols, labels=scale_labs) +
    scale_shape_manual('', breaks=scale_breaks, values=scale_pch, labels=scale_labs) + 
    scale_linetype_manual('', breaks=scale_breaks, values=scale_lty, labels=scale_labs) +
    theme_classic() +
    xlab('Date') +
    ylab(parse(text=switch(v, GPP='GPP~(gO[2]~m^-2~d^-1)', ER='ER~(gO[2]~m^-2~d^-1)', K600='K600~(d^-1)')))
}
g <- plot_gam(s='nwis_02234000', v='ER')
ggsave(args$outfile, plot=g, width=6, height=3)