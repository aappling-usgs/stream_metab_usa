# stream_metab_usa

This project contains the core data management scripts of the [USGS Powell Center working group on continental-scale stream metabolism](https://powellcenter.usgs.gov/view-project/54330b38e4b095098ca7d45d). The resulting data are now published in

Appling, A.P., Read, J.S., Winslow, L.A., Arroita, M., Bernhardt, E.S., Griffiths, N.A., Hall, R.O., Jr., Harvey, J.W., Heffernan, J.B., Stanley, E.H., Stets, E.G., and Yackulic, C.B., 2018, Metabolism estimates for 356 U.S. rivers (2007-2017): U.S. Geological Survey data release. [https://doi.org/10.5066/F70864KX](https://doi.org/10.5066/F70864KX).

and described in

Appling, A.P., Read, J.S., Winslow, L.A., Arroita, M., Bernhardt, E.S., Griffiths, N.A., Hall, R.O., Jr., Harvey, J.W., Heffernan, J.B., Stanley, E.H., Stets, E.G., and Yackulic, C.B., 2018. The metabolic regimes of 356 rivers in the United States. Scientific Data, Volume 5, Article 180292. [https://doi.org/10.1038/sdata.2018.292](https://doi.org/10.1038/sdata.2018.292).

## Workflow

The workflow is documented and enforced in [remake/remake.yml](https://github.com/USGS-CIDA/stream_metab_usa/blob/master/remake/remake.yml),
which can be run with these call in the R console:

```r
setwd('remake')
remake::make()
setwd('..')
```

For development, it may be easiest to run one target from one makefile at a
time. (The 'remake.yml' makefile actually calls several other makefiles, all in
the 'remake' directory.) The function `remake_smu`, defined in 'remake/build.R',
can help. For example:

```r
source('remake/build.R')
sb_sites <- remake_smu('sb_sites', '1_site_data.yml')
wtr_nwis <- remake_smu('wtr_nwis', '1_timeseries.yml')
```

## Dependencies

The main USGS package dependencies for the stream_metab_usa R project are
`streamMetabolizer`, `sbtools`,`dataRetrieval`, `geoknife`, and `mda.streams`.
To install, execute the following code chunk.

```r
install.packages(
 c("dataRetrieval", "R.utils", "geoknife", "sbtools"),
 repos=c("http://owi.usgs.gov/R","http://cran.rstudio.com"),
 dependencies=TRUE, type="both")
devtools::install_github("USGS-R/mda.streams")
devtools::install_github("USGS-R/streamMetabolizer")
```


## Disclaimer

This software is in the public domain because it contains materials that originally came from the United States Geological Survey, an agency of the United States Department of Interior. For more information, see the official USGS copyright policy at http://www.usgs.gov/visual-id/credit_usgs.html#copyright
