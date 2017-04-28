#' write a zip file of model fit information for this model
make_model_fit <- function(model_out, outdir) {

  library(methods) # necessary when running via 'Rscript' call at command line
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(unitted)
  library(streamMetabolizer)
  library(mda.streams)
  
  # Get model info
  config_row <- get_info(model_out)$config %>%
    mutate(
      resolution = substring(strategy, 7),
      model_name = make_metab_model_name(title=make_metab_run_title(date=format(as.Date(date), '%y%m%d'), tag=tag, strategy=strategy), row=config.row, site=site))
  
  # make a subdirectory to hold all the files
  subdir <- file.path(outdir, config_row$model_name)
  if(!dir.exists(subdir)) dir.create(subdir)
  
  # add non-R files, one per element of the fit list and the filtered input data
  fitplus <- get_fit(model_out)
  valid_dates <- filter(fitplus$daily, valid_day)$date
  fitplus$data <- get_data(model_out) %>%
    filter(date %in% valid_dates) %>%
    select(solar.time, DO.obs, DO.sat, depth, temp.water, light, discharge)
  fitplus$data_daily <- get_data_daily(model_out) %>%
    filter(date %in% valid_dates) %>%
    select(date, discharge.daily)
  fitplus$specs <- get_specs(model_out) %>%
    { .[-which(names(.) == 'model_path')] } %>%
    { capture.output(dput(.)) }
  lapply(names(fitplus), function(fitname) {
    f <- fitplus[[fitname]]
    if(is.data.frame(f)) {
      write.table(f, file=file.path(subdir, sprintf("%s.tsv", fitname)), row.names = FALSE, sep='\t')
    } else {
      writeLines(f, con=file.path(subdir, sprintf("%s.txt", fitname)))
    }
  })
  
  # compress and delete the uncompressed folder
  tarfile <- paste0(basename(subdir), ' fit.tar.gz')
  wd <- getwd()
  setwd(file.path(subdir, '..'))
  utils::tar(tarfile = tarfile, files=basename(subdir), compression='gzip', compression_level = 6)
  setwd(wd)
  unlink(subdir, recursive=TRUE)
  
  return(tarfile)
}
