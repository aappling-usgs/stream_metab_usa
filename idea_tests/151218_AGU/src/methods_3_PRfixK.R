# Figure: Final predictions (Phase 3) compared to original ones (Phase 1)

if((manual=FALSE)) {
  args <- list(outfile='out/methods_3_PRfixK.png')
} else {
  source("../../p1_import/code/process_make_args.R")
  args <- process_make_args(c("outfile"))
}

library(dplyr)
library(ggplot2)

load('cache/preds_1_PRK.RData')
load('cache/preds_3_PR.RData')
#load('cache/clean_1_filter.RData')

fulldat <- bind_rows(
  preds_1_PRK, preds_3_PR) %>% #, 
  #   filter(clean_1_filter, all.keep) %>% 
  #     select_(.dots=names(preds_3_PR)) %>%
  #     mutate(strategy='PR_fixed_K_filter')) %>%
  mutate(year = as.numeric(format(local.date, "%Y")))

scale_breaks <- c('PRK_initial','PR_fixed_K')
scale_labs <- c('Phase 1','Phase 3') %>% setNames(scale_breaks)
scale_cols <- c('navy','orange') %>% setNames(scale_breaks)
scale_pch <- c(19, 19) %>% setNames(scale_breaks)
scale_lty <- c(0, 0) %>% setNames(scale_breaks)

plotdat <- fulldat %>%
  filter(site=='nwis_02234000',
         #year %in% c(2011:2014), 
         strategy %in% c("PRK_initial","PR_fixed_K")) #_filter

g <- ggplot(plotdat, aes(x=local.date, y=ER, color=strategy)) + 
  geom_point(aes(shape=strategy)) + 
  geom_line(aes(linetype=strategy)) + 
  scale_color_manual('', breaks=scale_breaks, values=scale_cols, labels=scale_labs) +
  scale_shape_manual('', breaks=scale_breaks, values=scale_pch, labels=scale_labs) + 
  scale_linetype_manual('', breaks=scale_breaks, values=scale_lty, labels=scale_labs) +
  ylim(-45,20) + 
  theme_classic() + ylab(parse(text='ER~(gO[2]~m^-2~d^-1)')) + xlab("Date")
ggsave(args$outfile, plot=g, width=6, height=3)
