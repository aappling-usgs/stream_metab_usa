library(devtools)

# sbtools ####
install_github("USGS-R/sbtools")
library(sbtools)

authenticate_sb("aappling@usgs.gov")
current_session()

query_item_identifier('mda_streams', type="ts_doobs", limit=1000)
query_item_identifier('mda_streams', type="ts_doobs", key="nwis_01194750")
query_item_identifier('mda_streams', type="site_root", limit=1000)$title
item_list_children(query_item_identifier('mda_streams', type="site_root", key="nwis_01194750")$id)

item_list_files("54fc94cde4b02419550de200")
item_file_download("54fc94cde4b02419550de200", dest_dir=".")
tbl <- read.table(gzfile("./fileb932d1be4c7.tsv.gz"), sep="\t", header=TRUE)

# powstreams ####
install_github("USGS-R/geoknife")
install_github("USGS-R/mda.streams")
install_github("jread-usgs/powstreams") # Warning message: package '' is not available (as a binary package for R version 3.1.3) 
library(powstreams)

list_sites()
tses <- lapply(list_timeseries("nwis_01473900"), function(var) load_timeseries("nwis_01473900", variable=var))
head(do.call(merge, c(tses[1:2], list(by="DateTime"))))
site_location("nwis_01473900")

DOQ_sites <- list_sites(with_timeseries=c("doobs", "disch"), session=current_session()) # session is optional; authenticate_sb() will have pretty much taken care of this. 
# authentication, either above or with session argument, has to be used whe you're posting something.

# mda.streams ####
library(mda.streams)
wms_endpoint <- get_watershed_WMS("nwis_01473900")
mfs_endpoint <- get_watershed_WFS("nwis_01473900")
