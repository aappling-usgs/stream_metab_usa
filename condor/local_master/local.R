## Use this script to manually start and manage a Condor cluster.

#### Connect ####

#' 1. SSH into HTCondor M000 (130.11.177.99), probably using PuTTY.
#' 
#' 2. If needed, copy files over to the user/condor_R_snow folder on the node,
#' probably using FileZilla. Key files for this script are condor.sub, which
#' calls simple.sh and passes .Renviron to child nodes.

library(parallel)
library(tidyr)
library(dplyr)

# reinstall packages that may be out of date locally
#devtools::install_github('aappling-usgs/mda.streams', ref='develop')
#devtools::install_github('aappling-usgs/streamMetabolizer', ref='develop')
library(mda.streams)
library(streamMetabolizer)

# Set our choice between a local or Condor run
cluster <- c("localhost", "condor")[2]

# Start a cluster
switch(
  cluster,
  "condor" = {
    # Start a cluster, wait for them to connect
    c1 = makePSOCKcluster(paste0('machine', 1:50), manual=TRUE, port=4043)
    #' At this point go to putty, cd into condor_R_snow.
    #' 
    #' 3. Edit simple.sh to contain MASTER=YOUR.LOCAL.IP.ADDRESS (can find from
    #' R with system("ipconfig"))
    #' 
    #' 4. Type condor_submit condor.sub. This will yield the text:
    #' 
    #' Submitting job(s).......................................................... 
    #' 60 job(s) submitted to cluster XXXX.
    #' 
    #' 5. Query for updates using condor_q. We're looking for jobs to appear in
    #' that list with the cluster ID declared above (whatever number replaces
    #' XXXX).
    #' 
    #' 6. We're also looking to see them show up in the local console as replies
    #' to the makePSOCKcluster call. If the local console appears to have
    #' stalled while condor_q suggests all jobs have been dispatched, you can
    #' run condor_submit condor.sub again to add more worker nodes. Any nodes
    #' that don't find a master just disappear.
    #' 
    #' 7. When the makePSOCKcluster call has finished, we're ready to make 
    #' clusterCalls.
  } ,
  "localhost" = {
    # Alternatively, start a local cluster. If installs are needed, start with a 
    # 1-node cluster to run clusterCall(c1, function(){ install_check(...) }))
    c1 = makePSOCKcluster(rep('localhost', 1))
  })


#### Test ####
source("condor/lib/hello_cluster.R")
hello_sessions(c1)[1:4]
nodes <- hello_nodes(c1)
hello_jobs(c1)

#### Configure ####

source("condor/lib/configure.R")

# send the package install function to all nodes
clusterExport(c1, 'install_check')

# Now actually install packages, checking for completion each time. already installed: methods, parallel
all((out <- bind_rows(clusterCall(c1, function(){ install_check('cran', 'devtools') })))$install_success)
all((out <- bind_rows(clusterCall(c1, function(){ install_check('cran', 'dplyr') })))$install_success) # also installs lazyeval
all((out <- bind_rows(clusterCall(c1, function(){ install_check('cran', 'httr') })))$install_success) # also installs jsonlite, stringr
all((out <- bind_rows(clusterCall(c1, function(){ install_check('cran', 'lubridate') })))$install_success)
all((out <- bind_rows(clusterCall(c1, function(){ install_check('cran', 'RCurl') })))$install_success)
all((out <- bind_rows(clusterCall(c1, function(){ install_check('cran', 'reshape2') })))$install_success)
#all((out <- bind_rows(clusterCall(c1, function(){ install_check('cran', 'rstan') })))$install_success)
#all((out <- bind_rows(clusterCall(c1, function(){ install_check('cran', 'runjags') })))$install_success)
all((out <- bind_rows(clusterCall(c1, function(){ install_check('cran', 'XML') })))$install_success)
all((out <- bind_rows(clusterCall(c1, function(){ install_check('gran', 'dataRetrieval') })))$install_success) # dependencies: reshape2, lubridate, plyr, dplyr
clusterCall(c1, function() { library(devtools) })
all((out <- bind_rows(clusterCall(c1, function(){ install_check('github', 'LakeMetabolizer', 'GLEON') })))$install_success)
all((out <- bind_rows(clusterCall(c1, function(){ install_check('github', 'unitted', 'appling') })))$install_success)
all((out <- bind_rows(clusterCall(c1, function(){ install_check('github', 'geoknife', 'USGS-R') })))$install_success)
all((out <- bind_rows(clusterCall(c1, function(){ install_check('github', 'sbtools', 'aappling-usgs') })))$install_success)
all((out <- bind_rows(clusterCall(c1, function(){ install_check('github', 'streamMetabolizer', 'aappling-usgs', ref='develop') })))$install_success)
all((out <- bind_rows(clusterCall(c1, function(){ install_check('github', 'mda.streams', 'aappling-usgs', ref='develop') })))$install_success)


# DIAGNOSTICS
# there should be a JAGS folder listed here for every node
unlist(clusterCall(c1, function() { "JAGS" %in% dir("/usr/local/lib") }))

# view the overall install status
pkg_needs <- c('dataRetrieval', 'devtools', 'dplyr', 'geoknife', 'httr', 'jsonlite', 
               'LakeMetabolizer', 'lazyeval', 'lubridate', 'mda.streams', 'methods', 'parallel', 'RCurl', 
               'rstan', 'runjags', 'sbtools', 'streamMetabolizer', 'stringr', 'unitted', 'XML') #'rjags', 
inst <- view_installed_packages(pkg_needs)#[,1:6]
good_nodes <- which(unlist(inst['runjags',]))
c1 <- c1[good_nodes]
length(c1)

#### Get to Business ####

# if we're operating locally, stop the 1-node cluster and start a multi-node
# cluster
if(cluster=="localhost") {
  stopCluster(c1)
  # My machine has '8' nodes, which probably means 4 hyperthreaded nodes, so 6
  # may be a maximal use of those resources.
  c1 = makePSOCKcluster(rep('localhost', 6)) # for running models
}


#### Do Science ####

# this part goes in task-specific scripts.


#### End ####

stopCluster(c1)


#### Notes ####

# clusterExport(c1, 'somevar') sends somevar to every node

# to kill the cluster, in putty do condor_rm 1508 (e.g.) or (if you want log 
# files) call from R: stopCluster(c1); this will kill the nodes, and then condor
# should notice and clean itself up.