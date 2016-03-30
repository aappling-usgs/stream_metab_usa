# Functions to prepare and plot map data

library(dplyr)
library(tidyr)
library(stringi)
library(ggmap)
library(ggplot2)
library(grid)

label_dates <- function(doy) {
  format(as.Date("2014-12-31") + as.difftime(doy, units='days'), "%b %d")
}
theme_cols <- c('forestgreen','navy')

# for combining aes and aes_string, as in http://stackoverflow.com/questions/28777626/how-do-i-combine-aes-and-aes-string-options
`+.uneval` <- function(a,b) {
  `class<-`(modifyList(a,b), "uneval")
}

plot_timing_map <- function(data, variable, label='Mean Date of\nPeak GPP', format_by=c('date','difftime'), theme_colors=theme_cols) {
  # states outlines in background
  all_states <- map_data("state")
  g <- ggplot() + geom_polygon(data=all_states, aes(x=long, y=lat, group=group), color="lightgrey", fill='white')
  
  # no borders, axis labels, etc.
  g <- g + theme_classic() + 
    theme(axis.line = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),
          axis.ticks = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(),
          legend.position = c(1.1,0.7), plot.margin=unit(c(0,2,0.5,0), 'in'))
    
  # data, colored either by day of year or by length in days
  format_by <- match.arg(format_by)
  switch(
    format_by,
    date={
      date_breaks <- as.numeric(format(as.Date(c("2015-03-21","2015-06-21","2015-09-21","2015-12-21")), "%j"))
      date_labels <- label_dates(date_breaks)
      g <- g + geom_point(data=data, aes_string(color=variable) + aes(x=lon, y=lat, group=NA), size=4, alpha=1) +
        scale_colour_gradientn(label, lim=c(1,365), colours = rainbow(7), breaks=date_breaks, labels=date_labels)
    },
   difftime={
      g <- g + geom_point(data=data, aes_string(color=variable) + aes(x=lon, y=lat, group=NA), size=4, alpha=1) +
        scale_colour_gradient(label, low='lightgray', high=theme_colors[c(gpp=1,er=2)[[stri_extract_last_regex(variable, "gpp|er")]]])
    }
  )
  g
}

plot_timing_hist <- function(data, variable, short_label, format_by=c('date','difftime'), theme_colors=theme_cols) {
  
  varwmet <- paste0(variable, '_', c('gpp','er'))
  data <- data %>% 
    select_(.dots=c('site',varwmet)) %>%
    gather_('mettype', 'metval', varwmet[2:1])
    
  g <- ggplot(data, aes(x=metval, fill=mettype)) + 
    geom_density(color=NA, alpha=0.7)
  format_by <- match.arg(format_by)
  switch(
    format_by,
    date={
      date_breaks <- as.numeric(format(as.Date(c("2015-03-21","2015-06-21","2015-09-21","2015-12-21")), "%j"))
      date_labels <- label_dates(date_breaks)
      g <- g + scale_x_continuous(lim=c(0,365), breaks=date_breaks, labels=date_labels)
    },
    difftime={
      g <- g + scale_x_continuous(lim=c(0,365))
    }
  )
  g <- g + scale_fill_manual('', breaks=varwmet, values=setNames(theme_colors[1:2], varwmet), labels=setNames(c('GPP','ER'),varwmet)) +
    xlab(short_label) + ylab('Density') + 
    theme_classic() + theme(legend.position=c(0.2,0.9))
  g
}

plot_timing_EvG <- function(data, variable, short_label, format_by=c('date','difftime'), theme_colors=theme_cols) {
  
  varwmet <- paste0(variable, '_', c('gpp','er'))
  data <- data %>% 
    select_(.dots=c('site',varwmet))
  
  g <- ggplot(data, aes_string(x=varwmet[1], y=varwmet[2])) +
    geom_abline(aes(color='1:1 line'), linetype='dashed', show_guide = TRUE) +
    geom_point(color=theme_colors[2], size=1.6) +
    scale_color_manual('', values=c('1:1 line'=theme_colors[1])) +
    xlab(paste('GPP', short_label)) + ylab(paste('ER', short_label)) +
    theme_classic() + theme(legend.position=c(0.2,0.9), legend.background=element_blank()) +
    coord_fixed()
  
  xyrange <- range(unlist(data[varwmet]), na.rm=TRUE)
  format_by <- match.arg(format_by)
  switch(
    format_by,
    date={
      date_breaks <- as.numeric(format(as.Date(c("2015-03-21","2015-06-21","2015-09-21")), "%j"))
      date_labels <- label_dates(date_breaks)
      g <- g + scale_x_continuous(breaks=date_breaks, labels=date_labels, lim=xyrange) +
        scale_y_continuous(breaks=date_breaks, labels=date_labels, lim=xyrange)
    },
    difftime={
      g <- g + scale_x_continuous(lim=xyrange) + scale_y_continuous(lim=xyrange)
    }
  )
  g
  
}

plot_timing_reg <- function(data, variables, short_labels, format_by=c('date','difftime'), theme_colors=theme_cols) {
  
  data <- data %>% 
    select_(.dots=c('site',variables))

  g <- ggplot(data, aes_string(x=variables[2], y=variables[1])) + 
    geom_point(color=theme_colors[1]) + 
    geom_smooth(method='lm', color=theme_colors[2]) + 
    xlab(short_labels[2]) + ylab(short_labels[1]) +
    theme_classic()
  
  format_by <- match.arg(format_by)
  switch(
    format_by,
    date={
      date_breaks <- as.numeric(format(as.Date(c("2015-03-21","2015-06-21","2015-09-21")), "%j"))
      date_labels <- label_dates(date_breaks)
      g <- g + scale_y_continuous(lim=c(0,365), breaks=date_breaks, labels=date_labels)
    },
    difftime={
      g <- g + scale_y_continuous(lim=c(0,365))
    }
  )
  g
}

plot_timing_map_w_insets <- function(data, variable, label='Mean Date of\nPeak GPP', short_label='Peak Date', 
                              format_by=c('date','difftime')) {
  g <- plot_timing_map(data, variable, label, format_by=match.arg(format_by))
  h <- plot_timing_hist(data, variable, short_label, theme_colors)
  i <- plot_timing_reg(data, variable, short_label, theme_colors)
    
  print(g)
  print(h, vp=viewport(width = 0.3, height = 0.3, x = 0.15, y = 0.15))
  print(i, vp=viewport(width = 0.3, height = 0.3, x = 0.82, y = 0.15))
}