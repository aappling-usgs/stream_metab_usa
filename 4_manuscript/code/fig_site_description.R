# issue 186
# multi-panel fig:
#   National site map, dot size for length of record
#   distribution of stream discharge or watershed size
#   histograms of land use classes: urban vs ag vs everything else
#   Temporal coverage - density plot of record lengths

prep_spatial_points <- function(points, dailies) {
  site_nos <- mda.streams::parse_site_name(union(points$site_nm, unique(dailies$site_name)), out='sitenum')
  
  # use dataRetrieval to request annual statistics
  site_no_df <- data_frame(site_no=site_nos, chunk=rep(1:ceiling(length(site_no)/10), each=10)[seq_along(site_no)])
  mean_annual_disch <- purrr::map_df(unique(site_no_df$chunk), function(ch) {
    chunk_nos <- filter(site_no_df, chunk==ch) %>% pull(site_no)
    message(sprintf('chunk %d: %s', ch, paste(chunk_nos, collapse=', ')))
    suppressMessages(dataRetrieval::readNWISstat(
      chunk_nos, parameterCd='00060', startDate='2007', statReportType='annual', statType='mean'))
  })
  # combine annual stats into multi-year stats. results in only 329 measures;
  # other sites must lack sufficient data
  mean_disch <- mean_annual_disch %>%
    group_by(site_no) %>%
    summarize(madisch=mean(mean_va)*0.0283168) %>% # convert cfs to cms
    mutate(site_name=mda.streams::make_site_name(site_no, 'nwis')) %>%
    select(site_name, madisch) %>%
    ungroup()
  
  # use dataRetrieval to pull drainage areas (drain_area_va)
  site_info <- dataRetrieval::readNWISsite(site_nos) %>%
    mutate(site_name=mda.streams::make_site_name(site_no, 'nwis')) %>%
    left_join(dataRetrieval::stateCd, by=c('state_cd'='STATE')) %>%
    select(site_name, drain_area_va, contrib_drain_area_va, state=STUSAB, huc=huc_cd)
  
 # use the daily predictions/predictors table as yet another way to assess size
  # as mean of daily discharges on the days for which metabolism was estimated
  mean_metab_disch <- dailies %>%
    group_by(site_name) %>%
    summarize(
      mmdisch=mean(discharge),
      n_metab=length(GPP)) %>%
    ungroup()
  
  # combine the 3 above data sources into 1 table with several size measures
  combo_dat <- mean_metab_disch %>%
    full_join(mean_disch, by='site_name') %>%
    full_join(site_info, by='site_name')
  
  # count non-NA values
  # combo_dat %>%
  #   summarize(
  #     n_mmdisch=length(which(!is.na(mmdisch))),
  #     n_madisch=length(which(!is.na(madisch))),
  #     n_drain_area=length(which(!is.na(drain_area_va))),
  #     n_contrib_drain_area=length(which(!is.na(contrib_drain_area_va)))
  #   )
  # A tibble: 1 x 4
  # n_mmdisch n_madisch n_drain_area n_contrib_drain_area
  #     <int>     <int>        <int>                <int>
  #       356       329          337                   78
  
  # confirm that mmdisch and madisch are pretty similar, not too-too different from drain_area_va
  # cor(log(combo_dat$mmdisch), log(combo_dat$madisch), use='complete.obs') # 0.984
  # cor(log(combo_dat$drain_area_va), log(combo_dat$madisch), use='complete.obs') # 0.857
  # cor(log(combo_dat$drain_area_va), log(combo_dat$contrib_drain_area_va), use='complete.obs') # 0.997
  
  # because only mmdisch is available for every site, and madisch is similar and
  # higher quality, use madisch when available but gap fill with mmdisch
  plot_dat <- combo_dat %>%
    mutate(
      disch = ifelse(!is.na(madisch), madisch, mmdisch),
      logdisch=log(disch)) %>%
    select(site_nm=site_name, huc, n_metab, logdisch)
  
  # add the size info into the points st object
  points_prepped <- points
  points_prepped@data <- points@data %>%
    mutate(site_nm=as.character(site_nm)) %>%
    left_join(plot_dat, by='site_nm')
  
  return(points_prepped)
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

panel_spatial_points <- function(points_prepped) {
  # prepare aesthetic info (color, legend)
  points_w_aes <- points_prepped
  disch_breaks <- round(exp(quantile(points_w_aes@data$logdisch, probs=seq(0,1,by=0.25), na.rm=TRUE)), 1)
  points_legend <- list(
    #color = c('#ffffcc','#c2e699','#78c679','#238443'))
    color = c('#a1dab4','#41b6c4','#2c7fb8','#253494'))
    #color = c('#c2e699','#78c679','#31a354','#006837'))
  points_w_aes@data <- points_w_aes@data %>%
    mutate(
      disch_cut = cut(exp(points_w_aes@data$logdisch), breaks=disch_breaks),
      color=points_legend$color[disch_cut],
      color=ifelse(is.na(color), 'darkgrey', color))
  points_legend <- c(points_legend, list(
    label = data_frame(lo=disch_breaks[-length(disch_breaks)], hi=disch_breaks[-1]) %>%
      mutate(rng=sprintf('%s to %s', signif(lo, digits=2), signif(hi, digits=2))) %>%
      pull(rng)
  ))
  # project and plot - uses functions & globals from spatial_plotting.R
  par(new=TRUE, fig=c(0, 0.7, 0.4, 1))
  plot_national_site_map(sites=points_w_aes, state_col='grey93', sites_col=points_w_aes@data$color, sites_cex=1)
  par(new=TRUE, fig=c(0.7, 1, 0.4, 0.9), oma=c(0,0,0,0))
  plot.new()
  legend(title='Mean Discharge (cms)', x=0.05, y=0.95, legend=points_legend$label, col=points_legend$color, pch=20, bg=NA, bty='n', y.intersp=1, cex=0.9)
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

fig_site_description <- function(
  outfile='../4_manuscript/fig/sites.pdf',
  daily_zip='../4_data_release/cache/models/post/daily_predictions.zip',
  points_shp='../1_spatial/cache/points_shapefile/points_shapefile.shp',
  site_tsv='../4_data_release/cache/site_data.tsv') {
  
  # load the files
  unzipped <- unzip(zipfile=daily_zip, exdir=file.path(tempdir(), 'site_description'))
  dailies <- readr::read_tsv(unzipped)
  points <- readOGR(points_shp, layer='points_shapefile')
  site_info <- readr::read_tsv(site_tsv)
  
  # prepare the plotting data
  points_prepped <- prep_spatial_points(points, dailies)
  dailies_prepped <- prep_temporal_coverage(dailies)
  
  # start the plot file
  pdf(file = outfile, width = 6, height = 6)
  layout(matrix(data = 1, ncol = 1, byrow = TRUE))
  par(mar=c(0,0,0,0), omi=c(0.2,0,0,0))
  plot.new()
  
  # add subfigures
  panel_spatial_points(points_prepped)
  panel_temporal_coverage(dailies_prepped)
  
  # close the plot file
  dev.off()
  
}
