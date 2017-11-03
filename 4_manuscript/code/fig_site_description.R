
plot_site_description(outfile, dailies='../4_data_release/cache/models/post/daily_predictions.zip') {
  unzipped <- unzip(zipfile=dailies, exdir=file.path(tempdir(), 'site_description'))
  dailies <- readr::read_tsv(unzipped)
  
  counts <- dpb %>% group_by(site_name) %>% count()
  
  # plot(density(counts$n/365.25))
  
  quantile_lines <- data_frame(
    
  )
  
  counts %>% mutate(site_sort = ordered(site_name, levels=site_name[order(n)]), site_rank = as.numeric(site_sort)) %>% 
    ggplot(aes(x=site_rank, y=n/365.25)) + geom_point() + theme_bw() + 
    xlab("Site") + ylab("Number of daily predictions (years)") + scale_y_continuous(breaks=1:9, minor_breaks=NULL) + 
    geom_line(color='red')
  
}