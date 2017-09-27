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
#' step1 <- task_step(
#'   step_name = 'prep',
#'   target = function(task_name, step_name, ...) {
#'     sprintf('%s_%s', task_name, step_name)
#'   },
#'   depends = c('A','B'),
#'   command = 'process(target_name, I("C"))'
#' )
#' step2 <- task_step(
#'   step_name = 'plot',
#'   command = function(target_name, task_name, ...) {
#'     sprintf('visualize("%s")', task_name)
#'   }
#' )
#' step3 <- task_step('report')
#' task_plan <- create_task_plan(c('AZ','CA','CO'), list(step1, step2, step3))
create_task_plan <- function(coordinates, task_steps) {
  
  # munge the coordinates into a named list
  coordinates <- if(is.data.frame(coordinates)) {
    whisker::rowSplit(coordinates)
  } else if(is.list(coordinates)) {
    coordinates
  } else if(is.character(coordinates)) {
    setNames(as.list(rep(NA, length(coordinates))), coordinates)
  }
  
  # check that task_steps is a list of task_steps
  if(!is.list(task_steps)) {
    stop('task_steps must be a list')
  } else {
    if(any(!sapply(task_steps, function(ts) is(ts, 'task_step')))) {
      stop('task_steps must be a list of task_step objects (see ?task_step)')
    }
  }
  
  # prepare a list of the tasks and steps
  tasks <- list()
  for(i in seq_along(coordinates)) {
    task <- list(
      task_name = names(coordinates)[i],
      steps = list()
    )
    for(j in seq_along(task_steps)) {
      # isolate just the definitions for this step
      step_def <- task_steps[[j]]
      
      # every step is a list with step_name as the first element
      step <- list()
      step$step_name = step_def$step_name
      
      # use the user's step definitions to define the recipe
      step$target_name <- evaluate_step_element(step_def, 'target_name', task, step)
      step$depends <- evaluate_step_element(step_def, 'depends', task, step)
      step$command <- evaluate_step_element(step_def, 'command', task, step)
      
      # add a T/F to help whisker avoid empty depends blocks. move this to the makefile function soon
      step$has_depends <- length(step$depends) > 0
      
      # add this step to the task. wait until now to append because easier to
      # type/read 'step' than 'task$steps[[j]]' above
      task$steps[[step$step_name]] <- step
    }
    # append task to full tasks list. wait until now to append because faster
    # when coordinates is a long list
    tasks[[task$task_name]] <- task
  }
  
  return(tasks)
}

#' Evaluate a step element (target, depends, command) as a character
#' string/vector or as a function of the task and step elements (depending on the class of the element)
#'
#' @keywords internal
evaluate_step_element <- function(task_step, element, task, step) {
  # extract the desired task-step element (tse)
  if(!element %in% names(task_step)) {
    stop("task_step does not define the element '", element, "'")
  }
  tse <- task_step[[element]]
  
  # evaluate the tse according to its class
  if(is.character(tse)) {
    return(tse)
  } else if(is.function(tse)) {
    args <- c(task, step) # combine all available task-step info into a single list
    return(do.call(tse, args)) # apply tse as a function of that info
  }
}
#' Create an object that defines a step within a task
#'
#' The default values of each parameter are often acceptable, but all parameters
#' may be overridden. When constructing the task makefile or table, the
#' `target_name`, `depends`, and `command` elements are built in that order,
#' with each element optionally depending on the result of the previous
#' elements. They can also depend on the `step_name` (defined in this function call) and/or the `task_name` (defined in )
#'
#' @param step_name a single character string naming this step. The default
#'   `target_name` combines this `step_name` with the `task_name`, and the
#'   default `command` assumes this `step_name` is the function name, but both
#'   defaults may be overridden (see next arguments)
#' @param target_name a character string or vector, or a function that produces
#'   a character string or vector, giving a unique name for the remake target
#'   for a specific application of this step to a specific task. If a function,
#'   should accept `...` and other args optionally including `task_name` and
#'   `step_name`
#' @param depends a character string or vector, or a function that produces a
#'   character string or vector, defining any dependencies that need to be
#'   declared in addition to those implied by `command`. If a function, should
#'   accept `...` and other args optionally including `task_name`, `step_name`,
#'   and `target_name` args optionally including `task_name` and `step_name`
#' @param command a character string or vector, or a function that produces a
#'   character string or vector, defining the command to be run for each
#'   application of this step to a specific task. If a function, should accept
#'   `...` and other args optionally including `task_name`, `step_name`,
#'   `target_name`, and `depends`
#' @md
#' @export
#' @examples
#' task_step(target_name=function(task_name, step_name, ...) {
#'   sprintf('~/MyProjects/thisproject/%s_%s.png', task_name, step_name)
#' }, command='plot_site(target_name)')
task_step <- function(
  step_name,
  target_name = function(task_name, step_name, ...) {
    sprintf('%s_%s', task_name, step_name)
  },
  depends = character(0),
  command = function(task_name, step_name, ...) {
    sprintf('%s("%s")', step_name, task_name)
  }
) {
  
  # create the step definition
  step_def <- list(step_name=step_name, target_name=target_name, depends=depends, command=command)
  class(step_def) <- 'task_step'
  
  # check the inputs for proper formatting
  for(elem_name in names(step_def)) {
    element <- step_def[[elem_name]]
    if(is.character(element)) {
      # this is fine
    } else if(is.function(element)) {
      # check that the arguments are as expected
      given_args <- names(formals(element))
      expected_args <- c('...','task_name','step_name','target_name','depends')
      unknown_args <- setdiff(given_args, expected_args)
      if(length(unknown_args) > 0) {
        stop(paste0("unknown argument name[s] ",paste("'",unknown_args,"'", collapse=", ")," in '", elem_name, "' element"))
      }
      if(!('...' %in% given_args)) {
        stop(paste0("'...' must be included in arguments to ", elem_name))
      }
    } else {
      stop('each element must be either a character string/vector or a function')
    }
  }
  
  return(step_def)
}

#' Create a .yml makefile (for use with remake) for a set of tasks that together
#' form a single job
#' 
#' @examples
#' create_task_makefile(task_plan, packages='mda.streams')
create_task_makefile <- function(task_plan, job_target = 'all', 
                                 include=c(), packages=c(), sources=c(),
                                 makefile=NULL, template.file='../lib/task_makefile.mustache') {
  
  # prepare the variables to be rendered in the template
  # prepare the overall job task
  job <- list(
    target_name = job_target,
    depends = unlist(lapply(task_plan, function(task) lapply(task$steps, function(step) step$target_name)), use.names=FALSE)
  )
  params <- list(
    job = job,
    target_default = overall_target,
    include = include,
    has_include = length(include) > 0,
    packages = packages,
    has_packages = length(packages) > 0,
    sources = sources,
    has_sources = length(sources) > 0,
    tasks = task_plan
  )
  
  # remove names where they interfere with whisker.render
  params$tasks <- unname(params$tasks)
  for(task in seq_along(params$tasks)) {
    params$tasks[[task]]$steps <- unname(params$tasks[[task]]$steps)
    for(step in seq_along(params$tasks[[task]]$steps)) {
      params$tasks[[task]]$steps[[step]]$has_depends <- length(params$tasks[[task]]$steps[[step]]$depends) > 0
    }
  }

  # read the template
  template <- readLines(template.file)
  
  # render the template
  yml <- whisker::whisker.render(template, data=params)
  yml <- gsub('[\n]{3,}', '\\\n\\\n', yml) # reduce 3+ line breaks to just 2
  cat(yml)
}
