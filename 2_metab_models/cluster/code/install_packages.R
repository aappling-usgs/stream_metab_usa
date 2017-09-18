#' Installs and reports on packages, using ./bundle/ as the source of packages
#' for installation.
install_packages <- function(rlib='rLibs', oldonly=FALSE) {
  # update & install packages from the local repo. use an lapply to ensure
  # ordering of installation
  message('installing packages')
  oldpkgs <- rownames(old.packages(repos="file:bundle"))
  if(oldonly) {
    # the yeti solution, where up-to-date packages are the norm for each job
    toinstall <- oldpkgs
  } else {
    # the condor solution, where every package is likely to need an update every time
    prereqs <- c('BH', 'devtools', 'Rcpp', 'RcppEigen', 'dataRetrieval', 'geoknife', 'sbtools') # last 3 for mda.streams?
    keypkgs <-   c('dplyr', 'rstan', 'tidyr', 'ggplot2', 'cowplot', 'yaml', 'streamMetabolizer', 'mda.streams')
    toinstall <- c(setdiff(oldpkgs, c(prereqs, keypkgs)), prereqs, keypkgs)
  }
  print(toinstall)
  installout <- lapply(
    toinstall,
    install.packages, 
    repos="file:bundle", type="source", dependencies=c("Depends","Imports"), lib=rlib, 
    INSTALL_opts=c('--no-docs','--no-html')
  )
  
  # report on package versions
  message('describing installed packages')
  print(.libPaths())
  ip <- installed.packages()
  print(ip[order(rownames(ip)),c('LibPath','Version'),drop=FALSE])
  
  # describe session
  message('describing session')
  print(sessionInfo())
  print(devtools::session_info())
}
