
# report render
# source this file, authenticate, then run:
# render.gapanalysis('html')
render.gapanalysis <- function(output='html') {
  library(rmarkdown)
  output_dir <- file.path(getwd(), "gap_analysis")
  rmd_file <- file.path(getwd(), "gap_analysis", "powstream_gap_analysis.Rmd")
  out_file <- render(rmd_file, paste0(output,"_document"), output_dir = output_dir)
  return(out_file)
}

render.maps <- function(nm.state, output='html') {
  library(rmarkdown)
  state.cd <- stateCd$STUSAB[stateCd$STATE_NAME == nm.state]
  output_dir <- file.path(getwd(), "gap_analysis/maps")
  rmd_file <- file.path(getwd(), "gap_analysis", "powstream_maps.Rmd")
  out_file <- render(rmd_file, paste0(output,"_document"), output_dir = output_dir,
                     output_file = paste0("siteMap_", state.cd, ".", output))
  return(out_file)
}

# data loading

load.gapanalysis.data <- function(){
  sb_data <- load.data.sb()
  metabData <- load.data.metab()
  allData_order <- load.data.streamorder()
  allData_list <- list(metabData = metabData, allData_order = allData_order)
  allData_list <- append(allData_list, sb_data)
}

library(sbtools)
authenticate_sb()

load.data.sb <- function(){
  
  library(foreign)

  listofitems_files <- list(allData_pop = list(item="51fbee3fe4b04b00e3d891c0", file="Ac_popd10"), 
                            allData_land = list(item="534c35d0e4b0af6611b1d0d0", file="AC_NLCD11"),
                            allData_land_RE = list(item="534c1bd0e4b0858a36dc41eb", file="NLCD11"),
                            allData_run = list(item="560442f6e4b03bc34f544c50", file="RunOff_aggresults"),
                            allData_temp = list(item="54529d78e4b0d48d9fb06d9e", file="TMean"),
                            allData_precip = list(item="51fa9aefe4b0eb3daee2f22e", file="ppt30yr"))

  allData_list <- lapply(listofitems_files, function(x) {
    
    tryCatch({
      item_file_download(x$item, dest_dir=tempdir())
    }, error=function(e) {
      cat("got an error, but it's probably fine\n")
    })
    
    zipfile <- unzip(file.path(tempdir(), paste0(x$file, ".zip")), exdir=tempdir())
    
    read.dbf(zipfile)
    
  })
  
}

load.data.metab <- function(){
  
  library(powstreams)
  
  metabDataAll <- get_meta()
  metabData <- metabDataAll[which(metabDataAll$manual.assessment %in% c("accept", "examine")), ] 
  
}

load.data.streamorder <- function(){
  
  allData_order <- read.table("nhdplus_v2_streamorder", 
                              sep=";", header = TRUE)
  
}

load.data.latlon <- function(){
  
  library(readr)
  comidcoords <- read_csv("gap_analysis/test_net.csv")
  
  startcoords <- substr(comidcoords$startpoint, 10, nchar(comidcoords$startpoint)-3)
  startsplit <- strsplit(startcoords, " ")
  startlon <- as.numeric(sapply(startsplit, `[`, 1))
  startlat <- as.numeric(sapply(startsplit, `[`, 2))
  
  coords <- data.frame(comid=comidcoords$comid, lat=startlat, lon=startlon)
  
}


# data mining 

format.df <- function(metab_var, all_var, metabData, allData, type){
  metab_vals <- metabData[[metab_var]]
  
  if(!is.null(type) && type == "ac_ag") {
    all_vals <- allData[, all_var]/allData[, "ACLU_AREA"]
  } else if(!is.null(type) && type == "re_ag"){
    all_vals <- allData[, all_var]/allData[, "NLCD11_ARE"]
  } else if(!is.null(type) && type == "re_precip"){
    all_vals <- allData[, all_var]/1000
  } else {
    all_vals <- allData[, all_var]
  }

  df <- data.frame(data_type=c(rep("Metabolism Sites", length(metab_vals)),
                               rep("NHD+ Sites", length(all_vals))),
                   data_vals=c(v(metab_vals), all_vals))
  # df$data_type <- ordered(df$data_type, c("NHD+ Sites", "Metabolism Sites"))
}

format.catchment.df <- function(accum_df, reach_df){
  accum_df <- mutate(.data = accum_df, catchment = rep("Accumulated Watershed", 
                                                       length(accum_df$data_vals)))
  reach_df <- mutate(.data = reach_df, catchment = rep("Reach Catchment", 
                                                       length(reach_df$data_vals)))
  df <- rbind(accum_df, reach_df)
}

format.runoff.count <- function(df_run){
  sum_notincluded_zero <- df_run %>% filter(data_vals == 0) %>% 
    group_by(data_type, catchment) %>% summarize(count = n())
  sum_notincluded_zero <- sum_notincluded_zero %>% mutate(condition = rep("Runoff=0", nrow(sum_notincluded_zero)))
  
  sum_notincluded_low <- df_run %>% filter(data_vals < 0.1 & data_vals > 0) %>% 
    group_by(data_type, catchment) %>% summarize(count = n())
  sum_notincluded_low <- sum_notincluded_low %>% mutate(condition = rep("0<Runoff<0.1", nrow(sum_notincluded_low)))
  
  sum_notincluded <- rbind(sum_notincluded_zero, sum_notincluded_low)
  
  sum_notincluded$data_type <- as.character(sum_notincluded$data_type)
  sum_notincluded <- rbind(sum_notincluded, c("NHD+ & Metabolism", "Accum & Reach", 
                                              max(nrow(df_run %>% filter(catchment == "Accumulated Watershed")), 
                                                  nrow(df_run %>% filter(catchment == "Reach Catchment"))), 
                                              "All Runoff Values"))
  
  sum_notincluded <- sum_notincluded[, c("condition", "data_type", "catchment", "count")]
  colnames(sum_notincluded) <- c("Condition", "Data Source", "Catchment Type", "Count")
  return(sum_notincluded)
}

get.median <- function(df, type, catchment){
  #   vals <- df %>% 
  #     filter(data_type == type) %>% 
  #     filter(catchment == catchment) %>% 
  #     .$data_vals
  #   
  #   median(vals)
  
  vals <- df$data_vals[which(df$data_type == type & df$catchment == catchment)]
  med_val <- median(na.omit(vals))
  return(round(med_val, digits=3))
}

format.med.df <- function(df, reach){
  
  median_all_ac <- get.median(df, type="NHD+ Sites", catchment="Accumulated Watershed")
  median_metab_ac <- get.median(df, type="Metabolism Sites", catchment="Accumulated Watershed")
  median_df <- data.frame(data_vals = c(median_all_ac, median_metab_ac),
                          data_type = c("NHD+ Sites", "Metabolism Sites"),
                          catchment = rep("Accumulated Watershed", 2),
                          plot_labels = c(paste(median_all_ac, "(Accum)"),
                                          paste(median_metab_ac, "(Accum)")))
  
  if(reach){
    median_all_re <- get.median(df, type="NHD+ Sites", catchment="Reach Catchment")
    median_metab_re <- get.median(df, type="Metabolism Sites", catchment="Reach Catchment")
    median_df_re <- data.frame(data_vals = c(median_all_re, median_metab_re),
                               data_type = c("NHD+ Sites", "Metabolism Sites"),
                               catchment = rep("Reach Catchment", 2),
                               plot_labels = c(paste(median_all_re, "(Reach)"),
                                               paste(median_metab_re, "(Reach)")))
    median_df <- rbind(median_df, median_df_re)
  }
  
  return(median_df)  
}

plot.dens <- function(df, title, log, reach, type, xlabel){
  
  df$data_type <- ordered(df$data_type, c("NHD+ Sites", "Metabolism Sites"))
  
  median_df <- format.med.df(df, reach)
  median_df$data_type <- ordered(median_df$data_type, c("NHD+ Sites", "Metabolism Sites"))
  
  all_label <- paste("NHD+ Sites:\n", 
                     paste(filter(median_df, data_type=="NHD+ Sites") %>% 
                             .$plot_labels, collapse="\n "))
  metab_label <- paste("Metabolism Sites:\n", 
                       paste(filter(median_df, data_type=="Metabolism Sites") %>% 
                               .$plot_labels, collapse="\n "))
  
  densPlot <- ggplot() + 
    geom_density(data = df, alpha=0.5,
                 aes(x=data_vals, fill=data_type)) +
    ggtitle(title) + 
    labs(y="Density", x=xlabel) +
    geom_vline(data=median_df, show_guide = TRUE,
               aes(xintercept = data_vals, color = data_type)) +
    scale_fill_manual(name="Site Type",
                      breaks=c("NHD+ Sites", "Metabolism Sites"),
                      values=c("red", "blue")) +
    scale_colour_manual(name="Median Values",
                        breaks=c("NHD+ Sites", "Metabolism Sites"),
                        labels=c(all_label, metab_label),
                        values=c("red", "blue"))
  
  if(log && type != "runoff"){
    densPlot <- densPlot + scale_x_log10()
  } else if(log && type == "runoff"){
    densPlot <- densPlot + scale_x_log10(limits = c(0.1, NA))
  }
  
  if(reach){
    densPlot <- densPlot + facet_grid(catchment ~ .)
  }
  
  return(densPlot)
  
}

plot.orderHist <- function(allData_order, df_order){
  
  df_order$data_type <- ordered(df_order$data_type, c("NHD+ Sites", "Metabolism Sites"))
  
  stream_order_range <- range(allData_order$streamord)
  hist_breaks <- stream_order_range[1]:stream_order_range[2]
  
  median_df <- df_order %>% group_by(data_type) %>% summarize(Medians = median(data_vals)) 
  median_df$data_type <- ordered(median_df$data_type, c("NHD+ Sites", "Metabolism Sites"))
  all_label <- paste("NHD+ Sites:", median_df$Medians[which(median_df$data_type == "NHD+ Sites")])
  metab_label <- paste("Metabolism Sites:", median_df$Medians[which(median_df$data_type == "Metabolism Sites")])
  
  ggplot(data=df_order, aes(data_vals)) +
    geom_histogram(alpha = .5, binwidth=1, origin = -0.5,
                   col="black", aes(fill=data_type)) +
    geom_vline(data=median_df, show_guide = TRUE,
               aes(xintercept = Medians, color = data_type)) +
    scale_x_continuous(breaks=hist_breaks) +
    facet_grid(data_type ~ ., scales="free") +
    ggtitle("Strahler Stream Order Values") + 
    labs(y="Count", x="Stream Order") + 
    scale_fill_manual(name="Site Type",
                      breaks=c("NHD+ Sites", "Metabolism Sites"),
                      values=c("red", "blue")) +
    scale_colour_manual(name="Median Values",
                        breaks=c("NHD+ Sites", "Metabolism Sites"),
                        labels=c(all_label, metab_label),
                        values=c("red", "blue"))
}

eflow.mag7 <- function(sites){
  startDate <- "1900"
  endDate <- "2015"
  stats <- "magnifSeven"
  allstats <- c()
  
  for(x in sites){
    statsout <- ObservedStatsUSGS(x, startDate, endDate, stats)
    allstats <- rbind(allstats, statsout)
  }
  
  return(allstats)
}

eflowPlot <- function(stat_df, title){
  
  all_stat_df <- gather(stat_df, stat, value, -site_no, -min_date, -max_date, -comment)
  
  eflowPlot <- ggplot(all_stat_df, aes(x=stat, y=value)) + 
    geom_boxplot() +
    ggtitle(title) +
    scale_y_log10() +
    labs(x="Statistic", y="Value", title="Eflow Statistics")
  
  return(eflowPlot)
}

matchLatLon.comid <- function(data, latlon_data, metric_var){
  latlon_index <- match(data$COMID, latlon_data$comid)
  data <- data %>% 
    mutate(LAT = latlon_data$lat[latlon_index]) %>% 
    mutate(LON = latlon_data$lon[latlon_index]) %>% 
    filter(data[metric_var] == 0)
}

map.runoff.ggmap <- function(runoff_data, latlon_data){
  
  runoff_data <- matchLatLon.comid(runoff_data, latlon_data, "RUNOFF_AC")
  runoff_data <- matchLatLon.comid(runoff_data, latlon_data, "MEAN")
  
  library(ggmap)
  
  latitude_ctr <- 39.8282
  longitude_ctr <- -98.5795
  
  mapSetup <- get_map(location = c(lon = longitude_ctr, lat = latitude_ctr), 
                      zoom = 5, source = "google", maptype = "terrain")
  
  runoffMap <- ggmap(mapSetup, extent = "device") +
    geom_point(data = runoff_data, aes(x = LON, y = LAT), color = "red")
  runoffMap
  
}

map.runoff.leaflet <- function(runoff_data, latlon_data){
  
  runoff_data <- matchLatLon.comid(runoff_data, latlon_data, "RUNOFF_AC")
  
  library(leaflet)
  
  latitude_ctr <- 39.8282
  longitude_ctr <- -98.5795
  
  
  
}
