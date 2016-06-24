create_ts_table <- function(sites, config, file.out){
  var <- tail(strsplit(file.out,'[_.]')[[1]],3)[1]
  src <- tail(strsplit(file.out,'[_.]')[[1]],2)[1]
  ts.name <- make_ts_name(var, src)
  filepaths <- make_ts_path(sites, ts.name, version = config$version, folder = config$temp_dir)
  false.vect <- rep(FALSE, length(filepaths))
  time.st <- rep(config$times[1], length(filepaths))
  time.en <- rep(config$times[2], length(filepaths))
  site.table <- data.frame(filepath=filepaths, local=false.vect, remote=false.vect, no.data=false.vect, 
                           time.st=time.st, time.en=time.en)
  write_site_table(site.table, file.out)
  return(file.out)
}
write_site_table <- function(table, filename){
  write.table(table, file=filename, sep='\t', row.names=FALSE)
}
