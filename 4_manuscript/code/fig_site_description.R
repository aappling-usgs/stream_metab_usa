# issue 186
# multi-panel fig:
#   National site map, dot size for length of record
#   distribution of stream discharge or watershed size
#   histograms of land use classes: urban vs ag vs everything else
#   Temporal coverage - density plot of record lengths

plot_site_description(outfile, dailies='../4_data_release/cache/models/post/daily_predictions.zip') {
  unzipped <- unzip(zipfile=dailies, exdir=file.path(tempdir(), 'site_description'))
  dailies <- readr::read_tsv(unzipped)
  
  # simplify (for plotting, anyway) to start & end dates of contiguous runs of estimates
  runs <- dailies %>%
    group_by(site_name) %>%
    arrange(date) %>%
    do(., {
      thesedates <- .$date
      diffs <- as.numeric(diff(thesedates), units="days")
      as_data_frame(unclass(rle(diffs))) %>%
        mutate(
          end_pos = cumsum(lengths) + 1,
          start_pos = c(1, end_pos[-n()]),
          start_date = thesedates[start_pos],
          end_date = thesedates[end_pos]
        ) %>%
        filter(values==1) %>%
        select(start_date, end_date)
    }) %>%
    ungroup()
  # group and rank sites by record length and density
  counts <- dailies %>%
    group_by(site_name) %>%
    summarize(
      days = length(date),
      years = days / 365.25,
      start = min(date),
      end = max(date),
      span_days = as.numeric(end - start, units='days') + 1,
      span_years = span_days / 365.25,
      mid = start + as.difftime(span_days/2, units='days'),
      n_jan = length(date[format(date, '%m') == '01']),
      seasonal = if(n_jan/days < (1/24)) "Seasonal" else  "Year-Round",
      peryear = days / span_years,
      peryear_bin = cut(peryear, breaks=c(17,100,200,300,366))
    ) %>%
    ungroup() %>%
    group_by(seasonal) %>%
    arrange(start) %>%
    mutate(site_rank_seasonal = 1:n()) %>%
    ungroup() %>%
    arrange(start) %>%
    mutate(site_rank_all = 1:n())
  ranked_runs <- left_join(runs, counts, by='site_name')
  
  # FIGURE: Duration, seasonality, and frequency of daily metabolism observations
  ggplot(ranked_runs) +
    geom_segment(aes(x=start_date, xend=end_date, y=site_rank_all, yend=site_rank_all, color=peryear_bin)) +
    scale_colour_manual('Days per Year', values=c('#e66101','#fdb863','#b2abd2','#5e3c99')) + #http://colorbrewer2.org/#type=diverging&scheme=PuOr&n=4
    scale_y_continuous('Site Rank by Start Date', breaks=c(0,120,240,356)) +
    xlab('Date') +
    theme_classic() +
    theme(legend.position=c(0.2,0.75))
    
  counts <- dailies %>% group_by(site_name) %>% count()
  
  ggplot(dailies, aes(x=date, y=site_name)) + geom_point()
  
  # plot(density(counts$n/365.25))
  
  quantile_lines <- data_frame(
    
  )
  
  counts %>% mutate(site_sort = ordered(site_name, levels=site_name[order(n)]), site_rank = as.numeric(site_sort)) %>% 
    ggplot(aes(x=site_rank, y=n/365.25)) + geom_point() + theme_bw() + 
    xlab("Site") + ylab("Number of daily predictions (years)") + scale_y_continuous(breaks=1:9, minor_breaks=NULL) + 
    geom_line(color='red')
  
}