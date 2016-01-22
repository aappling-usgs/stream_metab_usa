# Figure: K predictions and observations

if((manual=FALSE)) {
  args <- list(outfile='out/methods_4_filter.png')
} else {
  source("../../p1_import/code/process_make_args.R")
  args <- process_make_args(c("outfile"))
}

library(ggplot2)
library(dplyr)

#' Plot the original versus final predictions
#' 
#' @param sitechoice a single site to plot
#' @param vars variable[s] to show
#' @param plot which ts of the var to plot. '.orig' will show the original and 
#'   final points, whereas '.prev' should just show the final points because 
#'   clean_1_filter is a late-iteration product where no further points should
#'   have been removed recently
plot_filter_stages <- function(clean_filter, sitechoice="nwis_06894000", vars=c('K600','ER','GPP'), plot=c('.orig','.prev'), keep_by='all') {
  plot <- match.arg(plot)
  if(length(keep_by) == 1) keep_by <- rep(keep_by,length(vars))
  lapply(vars, function(var) {
    ggplot(filter(clean_filter, site==sitechoice), 
           aes_string(x='local.date', y=paste0(var,plot), color=paste0(keep_by[match(var,vars)], '.keep'))) +
      geom_point(alpha=0.7) + 
      xlab('Date') +
      ylab(parse(text=switch(var, GPP='GPP~(gO[2]~m^-2~d^-1)', ER='ER~(gO[2]~m^-2~d^-1)', K600='K600~(d^-1)'))) + 
      scale_color_discrete("Keep") +
      theme_classic()
  })
}

load('cache/clean_1_filter.RData')

if((manual=FALSE)) {
  # random sample of sites from preds_3_PR: 
  # "nwis_03298150" "nwis_13213100" "nwis_02458450" "nwis_07056515"
  # "nwis_01570500" "nwis_01649190" "nwis_06601200" "nwis_04087030"
  # "nwis_03293530" "nwis_05418400"
  site = "nwis_06894000"
  vars = c('K600','ER','GPP')
  plot_filter_stages(clean_1_filter, site, keep_by='all') # see all points
  plot_filter_stages(clean_1_filter, site, keep_by=vars) # see all points
  plot_filter_stages(clean_1_filter, site, plot='.prev') # zoom in on good points
  plot_filter_stages(clean_1_filter, site, plot='.prev', keep_by=vars) # zoom in on good points
  plot_filter_stages(clean_1_filter, site, plot='.prev', keep_by='GPP') # GPP and ER remove a lot of high K values
  plot_filter_stages(clean_1_filter, site, plot='.prev', keep_by='ER') # GPP and ER remove a lot of high K values
  plot_filter_stages(clean_1_filter, site, plot='.prev', keep_by='K600') # GPP and ER remove a lot of high K values
}

g <- plot_filter_stages(clean_1_filter, "nwis_06894000", vars='GPP', plot='.orig', keep_by='all')[[1]] + ylim(-5,8)
ggsave(args$outfile, plot=g, width=6, height=3)