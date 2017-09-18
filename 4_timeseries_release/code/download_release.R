
#' if file exists locally, skip. 
#' if file fails to get remote location, fail
download_release_ts <- function(file.path){
  
  parsed.path <- mda.streams::parse_ts_path(file.path, out = c("dir_name", "file_name", "var_src", "site_name"))
  mda.streams::download_ts(var_src = parsed.path$var_src, site_name = parsed.path$site_name, 
                           folder = parsed.path$dir_name, on_local_exists = 'skip', on_remote_missing = 'stop')
}


merge_and_post_sb <- function(target.name, parent.id, ...){
  
  tmpdir <- tempdir()
  zipfile <- file.path(tmpdir, paste0(target.name, '.zip'))
  
  rds.files <- c(...)
  tsv.files <- c()
  for (rds.file in rds.files){
    parsed.path <- mda.streams::parse_ts_path(rds.file, out = c("dir_name", "file_name", "var", "src", "site_name", "ts_name"))
    tsv.file <- make_ts_path(site_name = parsed.path$site_name, ts_name = parsed.path$ts_name, folder = tmpdir, version = 'tsv')
    tsv.file <- gsub(tsv.file,pattern = '.tsv.gz', replacement = '.tsv')
    write.table(x = unitted::deunitted(readRDS(rds.file)), file=tsv.file, sep='\t', row.names=FALSE, quote=TRUE)
    tsv.files <- c(tsv.files, tsv.file)
  }
  
  old.dir <- setwd(tmpdir)
  zip(zipfile = basename(zipfile), files = basename(tsv.files))
  setwd(old.dir)
  append_release_files(parent.id, zipfile)
  # zip those dogs and post
  unlink(c(zipfile, tsv.files))
}