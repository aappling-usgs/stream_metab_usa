attributes_metab_config <- function(
  unzipped='../2_metab_models/run3/out/config.tsv',
  attr.file='../4_data_release/in/attr_metab_config.csv') {
  
  # sketch out and read in the attribute table
  attribute_skeleton(unzipped, attr.file)
  attr_df <- readr::read_csv(attr.file, col_types = 'cccnnc')
  
  # learn about the data
  data_df <- readr::read_tsv(unzipped)
  
  # fill out the attribute table
  
  # write the final attribute table
  readr::write_csv(attr_df, path=attr.file)
}
  

attributes_metab_inputs <- function(
  zip.dir='../4_data_release/cache/models/post',
  attr.file='../4_data_release/in/attr_metab_inputs.csv') {
  
  unzipped <- unzip(zipfile=zip.file, exdir=file.path(tempdir(), 'inputs'))
  
  # input_files <- dir(dirname(object.zip), pattern='_input.zip$', full.names=TRUE)
  # # read and summarize each file
  # summaries <- bind_rows(lapply(input_files, function(zip.file) {
  #   message(zip.file)
  #   unzipped <- unzip(zipfile=zip.file, exdir=file.path(tempdir(), 'inputs'))
  #   input_df <- readr::read_tsv(unzipped, col_types = 'Tdddddd')
  #   unlink(unzipped)
  #   as_data_frame(lapply(input_df, range, na.rm=TRUE)) %>%
  #     mutate(
  #       site=gsub('_input\\.zip', '', basename(zip.file)),
  #       variable=c('min','max'))
  # }))
  # # do some data checks. some sites have some bad data, mostly not too bad  
  # filter(summaries, DO.obs < 0) # 5 sites with negative DO.obs, though seldom hugely negative
  # filter(summaries, DO.sat < 0) # 0 sites go negative
  # filter(summaries, depth < 0) # 0 sites go negative
  # filter(summaries, temp.water < -1) # nwis_01646500 has at least one -999999
  # filter(summaries, light < 0) # 0 sites go negative
  # filter(summaries, discharge < 0) # 32 sites go negative
  # 
  # # compute the overall range
  # ranges <- bind_rows(
  #   summarize_all(filter(summaries, variable=='min'), min),
  #   summarize_all(filter(summaries, variable=='max'), max)) %>%
  #   select(-site)
  # 
  # unzipped1 <- unzip(zipfile=input_files[1], exdir=file.path(tempdir(), 'input1'))
  # attribute_skeleton(object, attr.file, ...)
  # obj_df <- readr::read_tsv(object)
  # attr_df <- readr::read_csv(attr.file)
  
}

attributes_metab_fits <- function(
  zip.file='../4_data_release/cache/models/post/nwis_01124000_15min_fit.zip', # example file
  attr.file='../4_data_release/in/attr_metab_fits.csv') {
    
  unzippeds <- unzip(zipfile=zip.file, exdir=file.path(tempdir(), 'fits'))
  for(unzipped in unzippeds) {
    
  }
}

attributes_metab_diagnostics <- function(
  zip.file='../4_data_release/cache/models/post/diagnostics.zip',
  attr.file='../4_data_release/in/attr_metab_diagnostics.csv') {
  
  unzipped <- unzip(zipfile=zip.file, exdir=file.path(tempdir(), 'diagnostics'))
}

attributes_daily_preds <- function(
  zip.file='../4_data_release/cache/models/post/daily_predictions.zip',
  attr.file='../4_data_release/in/attr_metab_daily_preds.csv') {
  
  unzipped <- unzip(zipfile=zip.file, exdir=file.path(tempdir(), 'preds'))
}
