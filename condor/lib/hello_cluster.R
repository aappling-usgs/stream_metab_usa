#' Return the R session information from each node
hello_sessions <- function(c1) {
  clusterCall(c1, function(){ 
    message("message from this node to its own log file")
    return(sessionInfo())
  })
}

#' Return the system information from each node (packaged into a single
#' data.frame)
hello_nodes <- function(c1) {
  bind_rows(
    lapply(seq_len(length(c1)), function(i) {
    tryCatch({
      clusterCall(c1[i], function() { 
        info <- data.frame(active_node=TRUE, as.data.frame(t(Sys.info()), stringsAsFactors=FALSE))
        message("message from node ",info$nodename," to its own log file")
        return(info)
      })[[1]]
    }, error=function(e) {
      data.frame(active_node=FALSE)
    })
  }))
}

#' Proof of concept for clusterApplyLB: Execute a bunch of trivial jobs (each 
#' involving just a message and a return value) to show that job != node and
#' that jobs are distributed to nodes according to current node availability.
hello_jobs <- function(c1) {
  jobfun <- function(job_id){ 
    message("message from job ", job_id, " in a log file")
    return(paste0("hello world from job ", job_id))
  }
  clusterApplyLB(c1, 1:77, jobfun)
}

#' Explore the directories on each node by a call to list.files
#' 
#' Parameters are as in list.files.
#' 
#' @examples 
#' hello_files(c1)
#' hello_files(c1, path="rLibs")
#' hello_files(c1, recursive=TRUE, include.dirs=TRUE)
#' hello_files(c1, pattern="*.Rlog")
#' clusterCall(c1, function() list.files(path=list.files(pattern="Rtmp*")))
hello_files <- function(c1, ...) {
  clusterCall(c1, function() list.files(...))
}
