#' Returns the parameter grid from grid_apply
#' @param object object from grid_apply
#' @export
jobs <- function(object){
  attr(object, "arg_grid")
}

#' Runs a job on the head node
#' @param .sge_id job id (row of \code{jobs(object)})
#' @param .mc.cores number of cores to use
#' @param .script.name name of the script given in \code{setup}
#' @details
#' Runs \code{Rscript doone.R x y} at the command line where \code{x} is \code{.sge_id},
#' and \code{y} is \code{.mc.cores}.
#'
#' Note that long running jobs claiming many cores may be killed by
#' the administrators. This should only be used for short tests, longer tests
#' should be submitted to the cluster as usual.
#'
#' @seealso \code{submit}, \code{filter}
#' @export
test_job <- function(.sge_id=1, .mc.cores=1, .script.name="doone.R"){
  cmd <- paste0("Rscript ", .script.name, " ", .sge_id, " ", .mc.cores)
  mysys(cmd)
}


#' Add jobs (or rows) to argument grid
#' @param object grid_apply object
#' @param ... key value pairs for \code{.f} in \code{grid_apply}
#' @return \code{grid_apply} object with updated
#' @details If all original keys are not in ..., the values of these arguments are set
#' to \code{NA}
#' @export
add_jobs <- function(object, ...){
  arg_grid <- attr(object, "arg_grid")
  new_grid <- expand.grid(...)

  if(!is.null(arg_grid$.sge_id)){
    last <- max(arg_grid$.sge_id)
    new_grid$.sge_id <- (last+1):(last+nrow(new_grid))
    arg_grid <- rbind(arg_grid, new_grid)
  } else {
    arg_grid <- rbind(arg_grid, new_grid)
    arg_grid$.sge_id <- 1:nrow(arg_grid)
  }

  attr(object, "arg_grid") <- arg_grid
  return(object)
}

#' Filter a subset of jobs (rows) from argument grid and modify submission script
#'
#' Requires \code{dplyr}.
#'
#' @export
#' @param ... arguments to \code{dplyr::filter}
#' @inheritParams setup
filter_jobs <- function(object, ...,
                        .mc.cores=1,
                        .dir= getwd(),
                        .queue="long",
                        .script.name="doone.R",
                        .job.name="distributr",
                        .out.dir="SGE_Output",
                        .R.version="3.2.5",
                        .email.options="a",
                        .email.addr=NULL,
                        .shell="bash"){
  .dir <- paste0(.dir, "/")
  arg_grid <- jobs(object)
  if(is.null(arg_grid$.sge_id)){
    arg_grid$.sge_id <- 1:nrow(arg_grid)
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr needed for this function to work. Please install it.",
         call. = FALSE)
  }
  arg_grid <- dplyr::filter(arg_grid, ...)
  tasks <- unlist(arg_grid$.sge_id, use.names = FALSE)

  if(all(tasks == tasks[1]:tasks[length(tasks)])){
    tasks <- paste0(tasks[1], ":", tasks[length(tasks)])
  } else {
    tasks <- paste0(tasks, collapse=", ")
  }
  print(head(arg_grid))

  write_submit(.dir, script.name=.script.name,
               mc.cores=.mc.cores,
               tasks=tasks,
               job.name=.job.name,
               out.dir = .out.dir,
               email = .email.options,
               email.addr = .email.addr,
               R.version = .R.version,
               shell = .shell)

  attr(object, "arg_grid") <- arg_grid
  invisible(object)
}
