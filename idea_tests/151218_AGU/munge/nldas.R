# Example NLDAS data for map figure

if((manual=FALSE)) {
  args <- list(outfile='cache/nldas.RData')
} else {
  source("../../p1_import/code/process_make_args.R")
  args <- process_make_args(c("outfile"))
}

library(dplyr)
library(mda.streams)
library(rasterVis)

library(geoknife)
# choose data
#webdatasets=query('webdata')
#grep('NLDAS-2', sapply(webdatasets@group, function(ds) ds$title))
vars='sw'
p_code <- get_var_src_codes(src=="nldas",var%in%vars,!is.na(p_code),out="p_code")
times <- c('2014-05-15 20:00','2014-05-15 20:00')

# download data  
stencil <- webgeom("state") # gets all states
#stencil <- webgeom("state::Alabama,Arizona,Arkansas,California,Colorado,Connecticut,Delaware,Florida,Georgia,Idaho,Illinois,Indiana,Iowa,Kansas,Kentucky,Louisiana,Maine,Maryland,Massachussetts,Michigan,Minnesota,Mississippi,Missouri,Montana,Nebraska,Nevada,New Hampshire,New Jersey,New Mexico,New York,North Carolina,North Dakota,Ohio,Oklahoma,Oregon,Pennsylvania,Rhode Island,South Carolina,South Dakota,Tennessee,Texas,Utah,Vermont,Virginia,Washington,West Virginia,Wisconsin,Wyoming")
fabric <- webdata('nldas', variable=p_code, times=times)
knife <- webprocess(algorithm = list('OPeNDAP Subset'="gov.usgs.cida.gdp.wps.algorithm.FeatureCoverageOPeNDAPIntersectionAlgorithm"),
                    REQUIRE_FULL_COVERAGE='false')
job <- geoknife(stencil, fabric, knife, wait=TRUE, OUTPUT_TYPE='geotiff')
check(job)
file <- download(job, destination = 'out/data_2_NLDAS.zip', overwrite=TRUE)

# extract data
unzip(file, exdir='out/data_2_NLDAS')
geotiff <- 'out/data_2_NLDAS' %>% file.path(., dir(.))
library(rasterVis)
nldas <- raster(geotiff)

save(nldas, file=args$outfile)
