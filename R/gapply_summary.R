#' Print a summary of \code{gresults}
#' @param object gresults object
#' @param .reps number of reps to scale to
#' @param ... unused
#' @return
#'  Prints the first few conditions, a summary of conditions, and estimated time to scale up to x reps.
#'  Returns the estimated time invisibly.
#' @export
summary.gresults <- function(object, .reps=NULL, ...){
  cat("",fill=T)
  grid <- attr(object, "arg_grid")

  cat("Number of conditions: ", nrow(grid), fill=T)
  cat("Time:", attr(object, "time"), fill=T)

  cat("",fill=T)
  if(!is.null(attr(object,"time"))){
    cat("Estimated time for x reps:", fill=T)
    cat("Reps \t Time", fill=T)
    o <- estimate_time(object, nreps=.reps)
    for(i in 1:nrow(o)){ cat(paste0(o[i,],"\t"),fill=T)}
  }
  invisible(o)
}

#' @export
summary.gapply <- summary.gresults

#' @export
print.gapply <- function(x, ...){
  y <- x
  attr(y, "time") <- NULL
  attr(y, "arg_names") <- NULL
  attr(y, ".f") <- NULL
  attr(y, "arg_grid") <- NULL
  attr(y, ".args") <- NULL
  attr(y, "err") <- NULL
  attr(y, "warn") <- NULL
  attr(y, ".reps") <- NULL
  class(y) <- c("list")
  print(y, ...)
  return(x)
}

#' Estimate time for a given number of reps
#' @param object gapply object
#' @param nreps number of reps to scale to
#' @export
estimate_time <- function(object, nreps=NULL){
  if(is.null(nreps)){ nreps <- c(50, 100, 500, 1000, 5000, 10000)}
  max.reps <- max(object$.rep)
  time.per.rep <- attr(object, "time")[3]/max.reps
  times <- lapply(time.per.rep * nreps, FUN = nicetime)
  o <- cbind(reps = nreps, times = times)
  return(o)
}

## From package astro
nicetime <- function (seconds) {
  lapseconds = round(seconds)
  seconds = lapseconds%%60
  minutes = ((lapseconds - seconds)/60)%%60
  hours = ((lapseconds - minutes * 60 - seconds)/3600)%%24
  days = ((lapseconds - hours * 3600 - minutes * 60 - seconds)/86400)
  lapline = {
  }
  if (days != 0) {
    if (days == 1) {
      lapline = paste(days, "d, ", sep = "")
    }
    else {
      lapline = paste(days, "d, ", sep = "")
    }
  }
  if (hours != 0 | days != 0) {
    if (hours == 1) {
      lapline = paste(lapline, hours, "h, ", sep = "")
    }
    else {
      lapline = paste(lapline, hours, "h, ", sep = "")
    }
  }
  if (minutes != 0 | hours != 0 | days != 0) {
    if (minutes == 1) {
      lapline = paste(lapline, minutes, "m, ", sep = "")
    }
    else {
      lapline = paste(lapline, minutes, "m, ", sep = "")
    }
  }
  if (seconds == 1) {
    lapline = paste(lapline, seconds, "s", sep = "")
  }
  else {
    lapline = paste(lapline, seconds, "s", sep = "")
  }
  return(lapline)
}
