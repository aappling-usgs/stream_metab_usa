
clusterCall(c1, function() { library(mda.streams) })
clusterCall(c1, function() { library(sbtools) })

## manually define SBUSER and SBPASS in console

# choose sites - currently specific to depth_calcDischHarvey
library(mda.streams)
dvqcoefs <- get_meta('dvqcoefs')
dvqcoefs <- dvqcoefs[complete.cases(dvqcoefs[c('dvqcoefs.k','dvqcoefs.m')]),]
sites <- dvqcoefs$site_name #list_sites()
sites=intersect(sites, list_sites('disch_nwis'))

# here for depth_calcDischRaymond
sites <- list_sites('disch_nwis')

# here for par_calcSw
sites <- list_sites('sw_nldas')

args <- list(var_src='par_calcSw', sites=sites, on_exists='skip', sb_user=SBUSER, sb_password=SBPASS, verbose=TRUE)
clusterExport(c1, 'args')
## manually source the add_calc_data function (but not the call to it)
clusterExport(c1, 'add_calc_data')
condor_add_calc_data <- function(sitenum) {
  add_calc_data(var_src=args$var_src, sites=args$sites[sitenum], on_exists=args$on_exists, 
                sb_user=args$sb_user, sb_password=args$sb_password, verbose=TRUE)
}
site_nums <- 1:length(args$sites)
all_out <- clusterApplyLB(c1, site_nums, condor_add_calc_data)

# the above seems to take some babysitting. for depth_calcDischHarvey, iterate on these:
length(is_posted <- list_sites('par_calcSw'))
args$sites <- args$sites[-na.omit(match(is_posted, args$sites))]
clusterExport(c1, 'args')
site_nums <- 1:length(args$sites)
all_out <- clusterApplyLB(c1, site_nums, condor_add_calc_data)

# and if you need to edit mda.streams:
clusterCall(c1, function() { 
  detach(package:mda.streams,unload=TRUE)
  install_check('github','mda.streams','aappling-usgs')
  library(mda.streams)
})

clusterCall(c1, function() { 
  sbtools::authenticate_sb(args$sb_user, args$sb_password)
})