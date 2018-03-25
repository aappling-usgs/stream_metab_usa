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
  year_breaks <- c(min(points_w_aes@data$years, na.rm=TRUE), 1,3,5,7, max(points_w_aes@data$years, na.rm=TRUE))
  year_colors <- viridisLite::viridis(length(year_breaks)-1, alpha=1, begin=0, end=1, direction=-1)
  points_w_aes@data <- points_w_aes@data %>%
    mutate(
      size = 0.6,
      year_bin = cut(years, breaks=year_breaks),
      year_bin_num = as.numeric(year_bin),
      color = year_colors[year_bin_num]
    )
  points_legend <- list(
    color = year_colors,
    label = data_frame(lo=year_breaks[-length(year_breaks)], hi=year_breaks[-1]) %>%
      mutate(rng=sprintf('%s to %s', signif(lo, digits=2), signif(hi, digits=3))) %>%
      pull(rng))
  # project and plot - uses functions & globals from spatial_plotting.R
  par(new=TRUE, fig=c(0, 0.7, 0, 1))
  plot_national_site_map(
    sites=points_w_aes,
    state_col='grey79', state_border='grey88',
    col=points_w_aes@data$color,
    bg='transparent',
    cex=points_w_aes@data$size,
    pch=19
  )
  par(new=TRUE, fig=c(0.7, 1, 0.3, 0.84), oma=c(0,0,0,0))
  plot.new()
  legend(title='Number of metabolism\nestimates in years', x=0.05, y=0.95,
         legend=points_legend$label, 
         pch=19, col=points_legend$color, bg=points_legend$color,#'transparent',
         bty='n', y.intersp=1, pt.cex=0.6, cex=0.8)
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
  pdf(file = outfile, width = 7, height = 3)
  par(mar=c(0,0,0,0), omi=c(0.2,0,0,0))
  plot.new()
  
  # add subfigures
  panel_spatial_points(points_prepped)
  
  # close the plot file
  dev.off()
}


#### not used ####

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
          end_date = thesedates[end_pos]
        )
    }) %>%
    filter(values==1) %>%
    select(site_name, start_date, end_date)
  # find the single days with multi-day gaps on either side
  solodays <- dailies %>%
    group_by(site_name) %>%
    arrange(date) %>%
    do(., {
      thesedates <- .$date
      diffs <- as.numeric(diff(thesedates), units="days")
      solodates <- thesedates[which(diffs > 1 & lag(diffs) > 1)]
      data_frame(start_date=solodates, end_date=solodates+0.5)
    })
  # combine
  runlines <- bind_rows(longruns, solodays) %>%
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
  
  # combine runlines and site rankings
  ranked_runs <- left_join(runlines, counts, by='site_name')
  
  return(ranked_runs)
}

panel_temporal_coverage <- function(dailies_prepped) { 
  # prepare data and dummy data for generating plot in base R
  lims_df <- data.frame(
    date = c(min(runlines$start_date), max(runlines$end_date)),
    y = c(0,nrow(counts)),
    dummy = NA)
  bounds_df <- data.frame(
    date = as.Date(sprintf('%d-10-01', seq(2007, 2016, by=3))))
  runs_legend <- list(
    label = gsub(',', '-', gsub('\\(|\\]', '', levels(ranked_runs$peryear_bin))),
    color = c('#e66101','#fdb863','#b2abd2','#5e3c99'),
    x = min(lims_df$date) + 1.2*diff(range(lims_df$date)),
    y = min(lims_df$y) + 1*diff(range(lims_df$y)))
  colored_runs <- ranked_runs %>%
    mutate(peryear_col = runs_legend$color[peryear_bin])
  
  # Plot: Duration and frequency of daily metabolism observations
  par(new=TRUE, mar=c(1.5, 4, 0.25, 12), fig=c(0, 1, 0.1, 0.5), xpd=TRUE)
  plot(dummy ~ date, data=lims_df, ylim=range(lims_df$y), yaxt='n', xaxt='n', xlab=NA, ylab='Site Rank by Start Date', bty='l', cex.lab=0.9)
  axis(1, bounds_df$date, format(bounds_df$date, '%Y-%m'), cex.axis=0.9)
  axis(2, c(0,120,240,356), cex.axis=0.9)
  segments(
    x0=colored_runs$start_date, x1=colored_runs$end_date,
    y0=colored_runs$site_rank_all, y1=colored_runs$site_rank_all,
    col=colored_runs$peryear_col)
  legend(title='Values per Year', x=runs_legend$x, y=runs_legend$y, legend=runs_legend$label, col=runs_legend$color, lty=1, bg=NA, bty='n', y.intersp=1, cex=0.9)
  
  # ggplot(ranked_runs) +
  #   geom_segment(aes(x=start_date, xend=end_date, y=site_rank_all, yend=site_rank_all, color=peryear_bin)) +
  #   scale_colour_manual('Days per Year', values=c('#e66101','#fdb863','#b2abd2','#5e3c99')) + #http://colorbrewer2.org/#type=diverging&scheme=PuOr&n=4
  #   scale_y_continuous('Site Rank by Start Date', breaks=c(0,120,240,356)) +
  #   xlab('Date') +
  #   theme_classic() +
  #   theme(legend.position=c(0.2,0.75))
}

# points_w_aes@data %>% ggplot(aes(y=years*365.25, x=span_years)) +
#   geom_abline(slope=365.25) +
#   geom_point() + theme_classic() +
#   xlab('Span of metabolism estimates (years)') +
#   ylab('Count of daily metabolism estimates (days)')
