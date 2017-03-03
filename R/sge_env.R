#' SGE environmental variables
#' @details These functions return environmental variables set by SGE with each job.
#' @name sge_env
NULL


# SGE_TASK_ID
#' @export
#' @describeIn sge_env returns current \code{SGE_TASK_ID}
sge_task_id <- function(){
  as.numeric(Sys.getenv("SGE_TASK_ID"))
}

# NSLOTS
#' @export
#' @describeIn sge_env returns current number of slots
sge_slots <- function(){
  as.numeric(Sys.getenv("NSLOTS"))
}

# JOB_ID
#' @export
#' @describeIn sge_env returns the current job id
sge_job_id <- function(){
  Sys.getenv("JOB_ID")
}

# JOB_NAME
#' @export
#' @describeIn sge_env returns the job name
sge_job_name <- function(){
  Sys.getenv("JOB_NAME")
}

#' @export
#' @describeIn sge_env is TRUE if running on SGE
on_sge <- function(){
  !is.na(sge_task_id())
}
