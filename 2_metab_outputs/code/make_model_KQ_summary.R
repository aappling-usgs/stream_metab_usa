fitdir <- '../2_metab_outputs/out/fits/'
fitfiles <- dir(fitdir, pattern='\\.tar\\.gz', full.names = TRUE)

ff <- bind_rows(lapply(fitfiles, function(fitfile) {
  # unzip the fit
  fit <- untar(fitfile, exdir = fitdir)
  fitunzipdir <- gsub(' fit.tar.gz', '', fitfile)
  
  # read the KQ file (K values)
  kqfile <- file.path(fitunzipdir, 'KQ_binned.tsv')
  kq <- read.table(kqfile, sep='\t', header=TRUE, stringsAsFactors=FALSE)
  
  # read the specs file (Q values)
  spfile <- file.path(gsub(' fit.tar.gz', '', fitfile), 'specs.txt')
  sp <- dget(spfile)
  
  # get the site name and model resolution
  mminfo <- mda.streams::parse_metab_model_name(basename(fitunzipdir))
  
  # create the df
  out <- kq %>%
    select(
      K600_log_mean=lnK600_lnQ_nodes_mean,
      K600_log_sd=lnK600_lnQ_nodes_sd) %>%
    mutate(
      Q_m3s_log=sp$K600_lnQ_nodes_centers,
      site=mminfo$site, 
      resolution=substring(mminfo$strategy, 7)) %>%
    select(site, resolution, Q_m3s_log, K600_log_mean, K600_log_sd)
  
  # clean up
  unlink(fitunzipdir, recursive=TRUE)
  
  # return
  out
}))
saveRDS(ff, 'KQ_fits.Rds')
