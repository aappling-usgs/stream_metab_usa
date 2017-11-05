ms_stats <- function(dailies='../4_data_release/cache/models/post/daily_predictions.zip') {
  unzipped <- unzip(zipfile=dailies, exdir=file.path(tempdir(), 'site_description'))
  dailies <- readr::read_tsv(unzipped)
  
  dates_per_site <- dailies %>% group_by(site_name) %>% count()
  
  range(dates_per_site$n) # 61 3296
  max(dates_per_site$n)/365.25 # 9.02
  median(dates_per_site$n)/365.25 # 3.229295
  mean(dates_per_site$n)/365.25 # 3.775365
  
  total_number_of_dates <- nrow(dailies) # 490907
}