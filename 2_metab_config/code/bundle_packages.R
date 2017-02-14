#' Creates a local repository
#' 
#' Keep the arguments describing package needs updated. each package listed here
#' must also be on CRAN or GRAN with its current dependencies listed there.
#' localpkgs should probably be listed both in remotepkgs (to get dependencies)
#' and localpkgs (to build from source)
#' 
#' @import miniCRAN
bundle_packages <- function(
  remotepkgs = c('streamMetabolizer', 'mda.streams', 'rstan', 'dplyr', 'tidyr', 'ggplot2', 'unitted', 'devtools', 'yaml'),
  repos = c('https://owi.usgs.gov/R','https://cran.rstudio.org'),
  localpkgs = paste0('../../', c('unitted', 'streamMetabolizer', 'mda.streams')),
  repoDir = '../2_metab_config/cluster/packages/bundle') {
  
  #### Run This ####
  
  library(miniCRAN)
  
  # get full list of package needs including dependencies. Omit BiocInstaller
  # because it's a pain to install this way
  pkgList <- setdiff(pkgDep(remotepkgs, repos=repos), 'BiocInstaller')
  
  # create or update a repository with all of the desired packages
  if(!dir.exists(repoDir)) {
    dir.create(file.path(repoDir, 'src/contrib'), recursive=TRUE)
    # prepare for bioconductor
    # source("https://bioconductor.org/biocLite.R")
    # biocLite()
    
    makeRepo(pkgList, path=repoDir, repos=repos, type=c('source'))
    # this error on updateRepoIndex appears to be sorta OK:
    # gzip: stdin: unexpected end of file
    # /usr/bin/tar: Unexpected EOF in archive
    # /usr/bin/tar: Error is not recoverable: exiting now
    failures <- setdiff(pkgList, rownames(pkgAvail(repos=repoDir, type="source")))
    while(length(failures) > 0) {
      message(paste0('Retrying adding these packages to the repo: ', paste0(failures, collapse=', ')))
      sapply(failures, addPackage, path=repoDir, type='source', repos=repos, deps=FALSE, writePACKAGES=FALSE)
      updateRepoIndex(repoDir, type=c("source"))
      failures <- setdiff(pkgList, rownames(pkgAvail(repos=repoDir, type="source")))
    }
  } else {
    updatePackages(path=repoDir, repos=repos, type="source", ask=FALSE) # should need update
  }
  
  # build some packages locally; replace any already in the miniCRAN repo with the
  # ones built here
  builds <- sapply(localpkgs, function(localpkg) {
    oldbuilds <- dir(file.path(repoDir, 'src/contrib'), pattern=basename(localpkg), full.names=TRUE)
    if(length(oldbuilds) > 0) file.remove(oldbuilds)
    devtools::build(localpkg, path=file.path(repoDir, 'src/contrib'), vignettes=FALSE)
  })
  updateRepoIndex(repoDir, type=c("source"))
  
  # zip the entire repository into a single file
  olddir <- getwd()
  on.exit(setwd(olddir))
  setwd(normalizePath(file.path(repoDir, '..')))
  zip('bundle.zip', 'bundle')
  setwd(olddir)
  
  message(paste0('the package files are all in ', repoDir, '.zip'))
  
  
  #### Run for Fun ####
  
  print(rownames(pkgAvail(repos=repoDir, type="source")))
  print(paste0('streamMetabolizer: ', pkgAvail(repos=repoDir, type="source")['streamMetabolizer','Version']))
  print(paste0('mda.streams: ', pkgAvail(repos=repoDir, type="source")['mda.streams','Version']))
  
  # return the file path to bundle
  return(paste0(repoDir, '.zip'))
}

