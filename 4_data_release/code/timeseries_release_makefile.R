create_timeseries_release_makefile <- function(makefile){
  
  ts.dir <- "../1_timeseries/out"
  
  list_files <- create_task_step(
    step_name = 'ts_files',
    command = sprintf('ts_files_for_download(target_name, I(\'%s\'))',ts.dir),
    depends = file.path(ts.dir, dir(ts.dir))
  )
  
  download_ts <- create_task_step(
    step_name = 'ts_downloaded',
    command = function(target_name, task_name, ...) {
      sprintf('download_release_tses(%s_ts_files)', task_name)
    }
  )
  
  timeseries <- create_task_step(
    step_name = '.dummy',
    command = function(target_name, task_name, ...) {
      sprintf('post_release_tses(target_name, pc1_timeseries, %s_ts_downloaded)', task_name)
    },
    target_name = function(task_name, ...){
      sprintf('%s_timeseries', task_name)
    }
  )
  
  # get unique sites:
  ts.files <- dir(ts.dir, full.names=TRUE) %>% {.[!grepl('all_ts_files|Thumbs.db', .)]}
  ts.sites <- c()
  library(dplyr)
  for (ts.file in ts.files){
    ts.file.sites <- readr::read_tsv(ts.file) %>% 
      mutate(site_name = mda.streams::parse_ts_path(file_path = filepath, out = "site_name")) %>% 
      pull(site_name)
    ts.sites <- unique(c(ts.sites, ts.file.sites))
  }
  
  task_plan <- create_task_plan(ts.sites, list(list_files, download_ts, timeseries))
  
  
  
  create_task_makefile(task_plan = task_plan, 
                       job_target = '4_release_timeseries', 
                       include = '4_release_parent.yml', 
                       packages = c('mda.streams', 'readr', 'dplyr'),
                       sources = c('../lib/load_profile.R','../4_timeseries_release/code/download_release.R', '../4_data_release/code/post_data_release.R'), 
                       makefile = makefile) 
  
}


