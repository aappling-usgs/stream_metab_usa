library(powstreams)
library(ggplot2)
library(dplyr)
library(tidyr)
library(unitted)

ts <- v(get_ts(c('sitetime_calcLon', 'dopsat_calcObsSat', 'disch_nwis', 'wtr_nwis', 'sw_nldas'), 'nwis_06893820'))
tsg <- gather(ts, var, value, dopsat, disch, sw, wtr)
between <- function(dates, t1, t2) { dates >= as.POSIXct(t1, tz=lubridate::tz(dates)) & dates < as.POSIXct(t2, tz=lubridate::tz(dates))}
ggplot(filter(tsg, between(sitetime, "2011-01-12", "2011-02-12")), aes(x=sitetime, y=value, color=var)) + geom_line() + facet_grid(var ~ ., scales='free_y') + theme_bw() + ylab("") + scale_color_discrete(guide=FALSE)
ggplot(filter(tsg, between(sitetime, "2011-04-01", "2011-04-25")), aes(x=sitetime, y=value, color=var)) + geom_line() + facet_grid(var ~ ., scales='free_y') + theme_bw() + ylab("") + scale_color_discrete(guide=FALSE)
