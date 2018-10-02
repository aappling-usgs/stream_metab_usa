# issue 186
# multi-panel fig:
#   National site map, dot size for length of record
#   distribution of stream discharge or watershed size
#   histograms of land use classes: urban vs ag vs everything else
#   Temporal coverage - density plot of record lengths
# but maybe we'll go a lot simpler

prep_spatial_points <- function(points, dailies, diagnostics) {
  site_nos <- mda.streams::parse_site_name(union(points$site_nm, unique(dailies$site_name)), out='sitenum')

  # use dataRetrieval to pull drainage areas (drain_area_va). many are NA
  site_info <- dataRetrieval::readNWISsite(site_nos) %>%
    filter(agency_cd == 'USGS') %>%
    mutate(site_name=mda.streams::make_site_name(site_no, 'nwis')) %>%
    left_join(dataRetrieval::stateCd, by=c('state_cd'='STATE')) %>%
    select(site_name, drain_area_va, contrib_drain_area_va, state=STUSAB, huc=huc_cd)

  # add in counts of total number of metabolism estimates and average per year
  metab_counts <- dailies %>%
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
      peryear = days / span_years
    )
  plot_dat <- left_join(site_info, metab_counts, by='site_name')
  
  # add the size info into the points st object
  points_prepped <- points
  points_prepped@data <- points@data %>%
    mutate(site_nm=as.character(site_nm)) %>%
    left_join(plot_dat, by=c('site_nm'='site_name'))
  
  return(points_prepped)
}


panel_spatial_points <- function(points_prepped) {
  # prepare aesthetic info (color, legend)
  points_w_aes <- points_prepped
  #year_breaks <- c(-0.00000000001, min(points_w_aes@data$years, na.rm=TRUE), 1,3,5,7, max(points_w_aes@data$years, na.rm=TRUE))
  day_breaks <- c(-0.00000000001, min(points_w_aes@data$days, na.rm=TRUE), 365, 1096, 1926, 2557, max(points_w_aes@data$days, na.rm=TRUE))
  day_colors <- c('grey60', viridisLite::viridis(length(day_breaks)-2, alpha=1, begin=0, end=1, direction=-1))
  points_w_aes@data <- points_w_aes@data %>%
    mutate(
      # years = ifelse(is.na(years), 0, years),
      # year_bin = cut(years, breaks=year_breaks),
      # year_bin_num = as.numeric(year_bin),
      days = ifelse(is.na(days), 0, days),
      day_bin = cut(days, breaks=year_breaks),
      day_bin_num = as.numeric(day_bin),
      color = day_colors[day_bin_num],
      shape = ifelse(day_bin_num == 1, 24, 19),
      size = ifelse(day_bin_num == 1, 0.4, 0.6)
    )
  points_legend <- list(
    color = day_colors,
    shape = c(24, rep(19, length(day_colors)-1)),
    size = c(0.4, rep(0.6, length(day_colors)-1)),
    label = data_frame(lo=day_breaks[-length(day_breaks)], hi=day_breaks[-1]) %>%
      #mutate(rng=sprintf('%s to %s', signif(lo, digits=2), signif(hi, digits=3)),
      mutate(rng=sprintf('%s to %s', round(lo), round(hi)),
             rng=c('0', rng[-1])) %>%
      pull(rng))
  # project and plot - uses functions & globals from spatial_plotting.R
  par(new=TRUE, fig=c(0, 0.7, 0, 1))
  plot_national_site_map(
    sites=points_w_aes,
    state_col='grey79', state_border='grey88',
    col=points_w_aes@data$color,
    bg='transparent',
    cex=points_w_aes@data$size,
    pch=points_w_aes@data$shape
  )
  par(new=TRUE, fig=c(0.7, 1, 0.24, 0.88), oma=c(0,0,0,0))
  plot.new()
  legend(title='Number of daily\nmetabolism estimates', x=0.05, y=0.95,
         legend=points_legend$label, 
         pch=points_legend$shape, pt.cex=points_legend$size*4/3,
         col=points_legend$color, bg=points_legend$color,
         bty='n', y.intersp=1, cex=0.8)
}

fig_site_description <- function(
  outfile='../4_manuscript/fig/sites.pdf',
  daily_zip='../4_data_release/cache/models/post/daily_predictions.zip',
  points_shp='../1_spatial/cache/points_shapefile/points_shapefile.shp',
  site_tsv='../4_data_release/cache/site_data.tsv') {
  
  # load the files
  unzipped <- unzip(zipfile=daily_zip, exdir=file.path(tempdir(), 'site_description'))
  dailies <- readr::read_tsv(unzipped)
  points <- rgdal::readOGR(points_shp, layer='points_shapefile')
  site_info <- readr::read_tsv(site_tsv)
  
  # prepare the plotting data
  points_prepped <- prep_spatial_points(points, dailies, diagnostics)
  
  # start the plot file
  pdf(file = outfile, width = 6, height = 3)
  par(mar=c(0,0,0,0), omi=c(0.2,0,0,0))
  plot.new()
  
  # add subfigures
  panel_spatial_points(points_prepped)
  
  # close the plot file
  dev.off()
}


prep_temporal_coverage <- function(dailies) {
  
  # identify start & end dates of contiguous runs of estimates so that obs can
  # be plotted as lines rather than points (lots faster)
  # find the multi-day runs of consecutive estimates
  longruns <- dailies %>%
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
          end_date = thesedates[end_pos],
          run='multiday'
        )
    }) %>%
    filter(values==1) %>%
    select(site_name, start_date, end_date, run)
  # find the single days with multi-day gaps on either side
  solodays <- dailies %>%
    group_by(site_name) %>%
    arrange(date) %>%
    do(., {
      thesedates <- .$date
      diffs <- as.numeric(diff(thesedates), units="days")
      solodates <- thesedates[which(diffs > 1 & (is.na(lag(diffs)) | lag(diffs) > 1))]
      data_frame(start_date=solodates, end_date=solodates+0.5, run='single')
    })
  # combine
  runlines <- bind_rows(longruns, solodays) %>%
    mutate(
      start_date = as.POSIXct(start_date) %>% lubridate::with_tz('UTC'),
      end_date = as.POSIXct(end_date) %>% lubridate::with_tz('UTC')) %>%
    arrange(site_name, start_date) %>%
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
      seasonal = if(n_jan/days < (1/24)) "Seasonal" else  "Non-seasonal",
      peryear = days / span_years,
      peryear_bin = cut(peryear, breaks=c(17,100,200,300,366))
    ) %>%
    ungroup() %>%
    mutate(seasonal = ordered(seasonal, c('Seasonal','Non-seasonal'))) %>%
    group_by(seasonal) %>%
    arrange(start, desc(peryear)) %>%
    mutate(site_rank_seasonal = 1:n()) %>%
    ungroup() %>%
    arrange(start, desc(peryear)) %>%
    mutate(site_rank_all = 1:n())
  
  # combine runlines and site rankings
  ranked_runs <- left_join(runlines, counts, by='site_name')
  
  return(ranked_runs)
}

panel_temporal_coverage <- function(dailies_prepped) { 
  
  # Plot: Duration and frequency of daily metabolism observations
  ggplot(dailies_prepped) +
    # one color:
    # geom_segment(aes(x=start_date, xend=end_date, y=site_rank_seasonal, yend=site_rank_seasonal), color='royalblue') +
    # four colors:
    # geom_segment(aes(x=start_date, xend=end_date, y=site_rank_seasonal, yend=site_rank_seasonal, color=peryear_bin), size=0.3) +
    # scale_colour_manual('Days per Year', values=c('#e66101','#fdb863','#b2abd2','#5e3c99')) + #http://colorbrewer2.org/#type=diverging&scheme=PuOr&n=4
    # many colors:
    geom_segment(aes(x=start_date, xend=end_date, y=site_rank_seasonal, yend=site_rank_seasonal, color=peryear), size=0.4) +
    scale_colour_gradient('Estimates per year', low='#fdb863', high='#5e3c99', limits=range(dailies_prepped$peryear)) +
    guides(colour = guide_colorbar(draw.ulim = FALSE,draw.llim = TRUE)) +
    scale_y_continuous('Site rank by start date') +
    xlab('Date') +
    theme_classic() +
    theme(legend.position=c(0.2,0.5), legend.key.height=unit(1, 'line'), strip.background=element_rect(fill=NA, color=NA)) +
    facet_grid(seasonal ~ ., scales='free_y', space='free_y')
}

fig_temporal_coverage <- function(
  outfile='../4_manuscript/fig/dates.pdf',
  daily_zip='../4_data_release/cache/models/post/daily_predictions.zip')
{
  # load the file
  unzipped <- unzip(zipfile=daily_zip, exdir=file.path(tempdir(), 'temporal_coverage'))
  dailies <- readr::read_tsv(
    unzipped, col_types = readr::cols(
      .default = col_double(),
      site_name = col_character(),
      resolution = col_integer(),
      date = col_date(format = "")
    ))
  
  # prepare the plotting data
  dailies_prepped <- prep_temporal_coverage(dailies)
  
  # plot and save
  g <- panel_temporal_coverage(dailies_prepped)
  ggsave(plot = g, filename = outfile, width = 5, height = 5.5)
}

# ggplot(dailies_prepped$ranked_runs) +
#   geom_segment(aes(x=start_date, xend=end_date, y=site_rank_all, yend=site_rank_all, color=peryear_bin)) +
#   scale_colour_manual('Days per Year', values=c('#e66101','#fdb863','#b2abd2','#5e3c99')) + #http://colorbrewer2.org/#type=diverging&scheme=PuOr&n=4
#   scale_y_continuous('Site Rank by Start Date', breaks=c(0,120,240,356)) +
#   xlab('Date') +
#   theme_classic() +
#   theme(legend.position=c(0.2,0.75))
# ggplot(dailies_prepped, aes(x=peryear)) + geom_density(color=NA, fill='gray') + theme_classic()
# dailies_prepped %>% ggplot(aes(y=years*365.25, x=span_years)) +
#   geom_abline(slope=365.25) +
#   geom_point() + theme_classic() +
#   xlab('Span of metabolism estimates (years)') +
#   ylab('Count of daily metabolism estimates (days)')
# dailies_prepped %>% ggplot(aes(y=peryear, x=span_years)) +
#   geom_hline(yintercept=365.25) +
#   geom_point() + theme_classic() +
#   xlab('Span of metabolism estimates (years)') +
#   ylab('Density of daily metabolism estimates\n(model dates divided by date range)')
