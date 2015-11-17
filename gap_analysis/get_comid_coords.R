# assumes you've downloaded test_net.csv to the gap_analysis directory

library(readr)
comidcoords <- read_csv("gap_analysis/test_net.csv")

startcoords <- substr(comidcoords$startpoint, 10, nchar(comidcoords$startpoint)-3)
startsplit <- strsplit(startcoords, " ")
startlon <- as.numeric(sapply(startsplit, `[`, 1))
startlat <- as.numeric(sapply(startsplit, `[`, 2))

coords <- data.frame(comid=comidcoords$comid, lat=startlat, lon=startlon)

library(ggplot2)
library(ggmap)
map <- get_map(location = c(lon = -115, lat = 45), zoom = 3, maptype = 'toner-lite')
ggmap(map) + geom_point(data=coords[sample.int(n=nrow(coords), size=1000),], aes(x=lon, y=lat), color='blue')
