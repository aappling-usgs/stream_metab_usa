#' Try to install a package and report on whether the installation was
#' successful
install_check <- function(repo, pkg, ghuser, ref, unload=TRUE) {
  
  warn_strs <- NA
  err_strs <- NA
  install_success <- FALSE
  if(missing(ref)) ref <- "master"
  
  # Try to install
  run_install <- function() {
    out <- capture.output(
      switch(repo,
             'cran'={ install.packages(pkg, repos='http://cran.rstudio.com') },
             'gran'={ install.packages(pkg, repos='http://owi.usgs.gov/R') },
             'github'={ install_github(paste0(ghuser, "/", pkg), ref=ref) }))
    if(!grepl("non-zero exit status", warn_strs)) install_success <<- TRUE
    paste0(out, collapse="")
  }
  out_strs <- withCallingHandlers(
    tryCatch(
      run_install(),
      error=function(e) { 
        err_strs <<- e$message
        NA 
      }),
    warning=function(w) { 
      warn_strs <<- if(!is.na(warn_strs)) paste0(warn_strs, "; ", w$message) else w$message
      invokeRestart("muffleWarning")
    })
  
  # Determine whether it is actually installed
  is_installed <- (pkg %in% unname(installed.packages()[,'Package']))
  
  # Report back
  return(data.frame(
    is_installed=is_installed, install_success=install_success, 
    output=out_strs, warnings=warn_strs, errors=err_strs, 
    stringsAsFactors=FALSE))
}

#' Determine whether the first elements in a list of lists are all TRUE
all_installs_complete <- function(out) {
  all(sapply(out, function(o) o$is_installed))
}

#' Print the second elements from a list of lists
view_install_notes <- function(out) {
  bind_rows(lapply(out, function(o) { as.data.frame(o, stringsAsFactors=FALSE) }))
}

# Function to see what we've achieved (or not)
view_installed_packages <- function(pkg_needs) {
  inst <- clusterCall(c1, function() { unname(installed.packages()[,"Package"]) })
  inst <- as.data.frame(
    lapply(inst, function(pkg_installed) pkg_needs %in% pkg_installed),
    stringsAsFactors=FALSE) %>%
    setNames(sapply(strsplit(hello_nodes(c1)$nodename, split=".", fixed = TRUE), function(node) node[1]))
  rownames(inst) <- pkg_needs
  inst
}

# Unload, reinstall, and reload a package
reinstall_github <- function(package, user='aappling-usgs', ref='master') {
  library(package, character.only=TRUE)
  tryCatch(detach(paste0('package:',package), unload=TRUE, character.only=TRUE), warning=function(w) stop(w))
  status <- install_check('github', package, user, ref)$install_success
  library(package, character.only=TRUE)
  status
}
