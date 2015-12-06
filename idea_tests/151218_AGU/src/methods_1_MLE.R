# Figure: DO predictions and observations, for illustration of MLE method

if((manual=FALSE)) {
  # manually define SBUSER & SBPASS
  args <- list(sb_user=SBUSER, sb_password=SBPASS, outfile='out/methods_1_MLE.png')
} else {
  source("../../p1_import/code/process_make_args.R")
  args <- process_make_args(c("sb_user", "sb_password", "outfile"))
}

library(mda.streams)
library(streamMetabolizer)
library(ggplot2)

sbtools::authenticate_sb(args$sb_user, args$sb_password)
#mms <- search_metab_models(tag='0.0.13') # get a list of models to choose from
mm <- get_metab_model("nwis_08068500-201-151125 0.0.13 PRK_initial" , update_sb=FALSE)
#range(predict_metab(mm)$local.date) # check the date range of the model we pulled
dopreds <- predict_DO(mm, "2014-04-15", "2014-04-15")

g <- ggplot(dopreds, aes(x=local.time)) + 
  geom_point(aes(y=DO.obs, shape='Observed', linetype='Observed'), color='blue', alpha=0.7) + 
  geom_line(aes(y=DO.mod, shape='Modeled', linetype='Modeled'), color='navy') + 
  scale_shape_manual('', values=c(NA, 19), guide='none') + 
  scale_linetype_manual('', values=c(1, 0), guide='none') +
  scale_x_datetime(labels=scales::date_format("%H:%M"), breaks="4 hours") +
  theme_classic() + ylab(parse(text="DO~(mg~O[2]~L^-1)")) + xlab("Time")
ggsave(filename=args$outfile, plot=g, width=4, height=3)
