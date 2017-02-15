#' Installs and reports on packages, using ./bundle/ as the source of packages
#' for installation.
install_packages <- function(rlib='rLibs') {
  # update & install packages from the local repo. use an lapply to ensure
  # ordering of installation
  message('installing packages')
  oldpkgs <- rownames(old.packages(repos="file:bundle"))
  prereqs <- c('devtools', 'Rcpp', 'RcppEigen')
  keypkgs <-   c('rstan', 'dplyr', 'tidyr', 'ggplot2', 'yaml', 'streamMetabolizer', 'mda.streams')
  toinstall <- c(setdiff(oldpkgs, c(prereqs, keypkgs)), prereqs, keypkgs)
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
