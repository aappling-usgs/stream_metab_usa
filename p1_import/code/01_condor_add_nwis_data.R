
clusterCall(c1, function() { library(mda.streams) })
clusterCall(c1, function() { library(sbtools) })

## manually define SBUSER and SBPASS in console
args <- list(sb_user=SBUSER, sb_password=SBPASS)
clusterExport(c1, 'args')
clusterCall(c1, function() { sbtools::authenticate_sb(args$sb_user, args$sb_password) })
needs_auth <- which(unlist(clusterCall(c1, function() { is.null(sbtools::current_session()) })))
clusterCall(c1[needs_auth], function() { sbtools::authenticate_sb(args$sb_user, args$sb_password) })
needs_auth <- which(unlist(clusterCall(c1, function() { is.null(sbtools::current_session()) })))

# choose sites - currently specific to depth_calcDischHarvey
library(mda.streams)
dvqcoefs <- get_meta('dvqcoefs')
dvqcoefs <- dvqcoefs[complete.cases(dvqcoefs[c('dvqcoefs.k','dvqcoefs.m')]),]
sites <- dvqcoefs$site_name #list_sites()
sites=intersect(sites, list_sites('disch_nwis'))

# here for veloc_calcDischRaymond
sites <- list_sites(c('veloc_calcDischRaymond','veloc_calcDischHarvey'), logic='any')

args <- list(var_src='velocdaily_calcDMean', sites=sites, on_exists='skip', sb_user=SBUSER, sb_password=SBPASS, verbose=TRUE)
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

# and if you need to edit mda.streams (but be careful - there's something strange about sbauth)
clusterCall(c1, function() { 
  detach(package:mda.streams,unload=TRUE)
  install_check('github','mda.streams','aappling-usgs')
  library(mda.streams)
})

# to delete a ts:
length(sites <- list_sites('K600_estDOMLEPRK'))
clusterExport(c1, 'sites')
site_nums <- 1:length(sites)
clusterApplyLB(c1, site_nums, function(i) { mda.streams:::delete_ts('gpp_estDOMLEPRK', sites[i]) })
