# Figure: Example of storms affecting metabolism

if((manual=FALSE)) {
  args <- list(outfile='out/seasons_1_example.png')
} else {
  source("../../p1_import/code/process_make_args.R")
  args <- process_make_args(c("outfile"))
}

source("src/example_lib.R")

#mda.streams::get_meta('basic') %>% dplyr::filter(site_name=='nwis_04136000')
egdat <- build_example('nwis_04136000', pred='sw_nldas', ag_fun='max', dates=as.Date(c("2009-01-01","2012-01-01")))
g <- plot_example(egdat, pred="sw", emphasize='slow', dates=as.Date(c("2009-01-01","2012-01-01")), title='Au Sable River, MI')
ggsave(args$outfile, plot=g, width=7, height=4)
