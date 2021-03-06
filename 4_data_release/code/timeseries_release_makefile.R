create_timeseries_release_plan <- function(release_sites) {
  
  ts.dir <- "../1_timeseries/out"
  
  list_files <- create_task_step(
    step_name = 'ts_files',
    command = sprintf('ts_files_for_download(target_name, I(\'%s\'))',ts.dir),
    depends = dir(ts.dir, pattern='files_ts_', full.names=TRUE)
  )
  
  download_ts <- create_task_step(
    step_name = 'ts_downloaded',
    command = function(target_name, task_name, ...) {
      sprintf('download_release_tses(%s_ts_files)', task_name)
    }
  )
  
  timeseries <- create_task_step(
    step_name = 'ts_posted',
    command = function(target_name, task_name, ...) {
      sprintf('post_release_tses(target_name, pc1_timeseries, %s_ts_downloaded)', task_name)
    },
    target_name = function(task_name, ...){
      sprintf('%s_timeseries', task_name)
    }
  )
  
  ind_dir <- '../4_data_release/log/timeseries'
  if(!dir.exists(ind_dir)) dir.create(ind_dir, recursive=TRUE)
  
  task_plan <- create_task_plan(
    release_sites, list(list_files, download_ts, timeseries),
    final_steps=c('ts_posted'), indicator_dir=ind_dir)
  
  return(task_plan)
}

create_timeseries_release_makefile <- function(makefile, task_plan, template_file) {
  create_task_makefile(
    makefile=makefile,
    task_plan=task_plan,
    indicator_dir= '../4_data_release/log',
    include='4_release_timeseries.yml',
    packages=c('mda.streams', 'readr', 'dplyr'),
    sources = c('../lib/load_profile.R','../4_data_release/code/timeseries_download_release.R', '../4_data_release/code/post_data_release.R'), 
    template_file=template_file)
}
