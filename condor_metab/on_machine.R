# Use this script to manually start and manage a Condor cluster.

# SSH into HTCondor M000 (130.11.177.99), probably using PuTTY

library(parallel)

#Start a cluster, wait for them to connect
c1 = makePSOCKcluster(paste0('machine', 1:50), manual=TRUE, port=4043)

# at this point go to putty, cd into condor_R_snow, and type condor_submit condor.sub

# query for updates using condor_q

clusterCall(c1, function(){install.packages('devtools', repos='http://cran.rstudio.com')})
clusterCall(c1, function(){install.packages('rLakeAnalyzer', repos='http://cran.rstudio.com')})
clusterCall(c1, function(){install.packages('dplyr', repos='http://cran.rstudio.com')})

clusterCall(c1, function(){library(devtools)})

glmr_install     = clusterCall(c1, function(){install_github('lawinslow/GLMr')})

# send the config file to each Condor node
configvar <- read.table(configfile)
clusterExport(c1, 'configvar')

run_ids = 1:656

do_something = function(run_id){
    
    tryCatch({
      ## do something
      Sys.sleep(10)
      # can reference configvar here.
      
      ## return results
      return(run_id*15)
    }, error=function(e){e})
}

# LB = load balance
out = clusterApplyLB(c1, run_ids, do_something)

# now write each element of out back out to files, if you want
for(o in out) {
  if(is.data.frame(o)) {
    write.table(o)
  } else {
    writeLines(o)
  }
  
}

# clusterExport(c1, 'somevar') sends somevar to every node

# to kill the cluster, in putty do condor_rm 1508 (e.g.) or (if you want log 
# files) call from R: stopCluster; this will kill the nodes, and then condor
# should notice and clean itself up.