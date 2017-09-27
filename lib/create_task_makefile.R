#' Create a templated remake YAML file that organizes tasks and their sub-tasks
#' for a large number of targets
#'
#' This function is a first draft of the scipiper function. Let's see how it
#' goes.
#'
#' @param makefile
#' @param include
#' @param packages
#' @param sources
#' @param job_target single character string naming the default target, which
#'   will include all tasks within the job
#' @param coordinates character vector of work coordinates, with length equal to
#'   the number of tasks. These character strings will be used to prefix all
#'   steps within a task. The prefix for each task should describe the aspect[s]
#'   of a task that position it within the larger parameter space of work to be
#'   done. Examples: site IDs, model names, character code describing specific
#'   parameterization of model
#' @param targets
#' @param depends
#' @param command
#' @param ...
#' @examples 
#' steps <- list(
#'   step1 = list(
#'     target = function(task_name, step_name, ...) {
#'       sprintf('%s_%s', task_name, step_name)
#'     },
#'     depends = c('A','B'),
#'     command = 'process(target_name)'
#'   ),
#'   step2 = list(
#'     command = function(target, ...) {
#'       sprintf('visualize(%s)', target)
#'     }
#'   )
#' )
#' task_plan <- create_task_plan(c('AZ','CA','CO'), .steps=steps)
step_defaults <- list(
  target = function(task_name, step_name, ...) {
    sprintf('%s_%s', task_name, step_name)
  },
  depends = character(0),
  command = function(step_name, ...) {
    sprintf('%s(target_name)', step_name)
  }
)
create_task_plan <- function(coordinates, ..., .steps, step_defaults=step_defaults) {
  
  # the steps definitions (steps_defs) can be passed in as [named] ... lists or
  # in a big list named .steps
  dotsteps <- list(...)
  steps_defs <- if(missing(.steps)) {
    dotsteps
  } else if(length(dotsteps) > 0) {
    stop("don't define both ... and .steps")
  } else {
    .steps
  }
  
  # munge the coordinates into a named list
  coordinates <- if(is.data.frame(coordinates)) {
    whisker::rowSplit(coordinates)
  } else if(is.list(coordinates)) {
    coordinates
  } else if(is.character(coordinates)) {
    setNames(as.list(rep(NA, length(coordinates))), coordinates)
  }
  
  # prepare a list of the tasks and steps
  tasks <- list()
  seq_coords <- seq_along(coordinates)
  seq_steps <- seq_along(steps_defs)
  for(i in seq_coords) {
    task <- list(
      task_name = names(coordinates)[i],
      steps = list()
    )
    for(j in seq_steps) {
      # isolate just the definitions for this step
      step_def <- steps_defs[[j]]
      
      # every step is a list with step_name as the first element
      step <- list()
      step$step_name = names(steps_defs)[[j]]
      
      # use the user's step definitions to define the recipe
      step$target <- evaluate_step_element(step_def$target, step_defaults$target, task, step)
      step$depends <- evaluate_step_element(step_def$depends, step_defaults$depends, task, step)
      step$command <- evaluate_step_element(step_def$command, step_defaults$command, task, step)
      
      # add a T/F to help whisker avoid empty depends blocks
      step$has_depends <- length(step$depends) > 0
      
      # add this step to the task. wait until now to append because easier to
      # type/read 'step' than 'task$steps[[j]]' above
      task$steps[[step$step_name]] <- step
    }
    tasks[[task$task_name]] <- task # wait until now to append to list because faster when coordinates is a long list
  }
  
  return(tasks)
}

create_task_makefile <- function(task_plan, include=c(), packages=c(), sources=c(), makefile=NULL, template.file='../lib/task_makefile.mustache') {
  
  # prepare the variables to be rendered in the template
  params <- list(
    target_default = 'all',
    include = include,
    has_include = length(include) > 0,
    packages = packages,
    has_packages = length(packages) > 0,
    sources = sources,
    has_sources = length(sources) > 0,
    tasks = task_plan
  )
  
  # read the template
  template <- readLines(template.file)
  
  # render the template
  yml <- whisker::whisker.render(template, data=params)
  yml <- gsub('[\n]{3,}', '\\\n\\\n', yml) # reduce 3+ line breaks to just 2
  cat(yml)
}
create_task_makefile(tasks, packages='mda.streams')

#' Evaluate a step element (target, depends, command) as a character
#' string/vector, function of the task and step elements, or a default
#'
#' @keywords internal
evaluate_step_element <- function(element, default, task, step) {
  if(is.null(element)) {
    return(evaluate_step_element(default, function(...) stop('default is undefined or NULL'), task, step))
  } else if(is.character(element)) {
    return(element)
  } else if(is.function(element)) {
    args <- c(task, step)
    return(do.call(element, args))
  }
}
