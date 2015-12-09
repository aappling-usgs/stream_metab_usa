# Figure: Example of storms affecting metabolism

if((manual=FALSE)) {
  args <- list(outfile='out/storms_1_example.png')
} else {
  source("../../p1_import/code/process_make_args.R")
  args <- process_make_args(c("outfile"))
}

source("src/example_lib.R")

egdat <- build_example('nwis_03034000', pred='disch_nwis', ag_fun='mean')
g <- plot_example(egdat, pred="disch", emphasize='quick', dates=as.Date(c("2014-04-21","2014-9-10")))
ggsave(args$outfile, plot=g, width=7, height=4)
