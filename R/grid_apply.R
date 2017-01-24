
#' @export
#' @describeIn gapply same as \code{gapply}, but does not simplify results,
#' leaving results as a list of vectors, lists, or data frames.
#' @param .paramid The index of the row of the argument grid to run.
#' If \code{NULL} (default), \code{.f} is evaluated for all arguments.
#' @importFrom purrr transpose
#' @importFrom parallel mclapply
grid_apply <- function(.f, ..., .reps=1, .args=NULL, .mc.cores=1, .verbose=1, .eval=T, .paramid=NULL){
  arg_grid <- expand.grid(...)
  arg.ls <- purrr::transpose(arg_grid)

  # Append the non-grid arguments
  arg.ls <- lapply(arg.ls, function(x, .args){append(x, .args)}, .args=.args)

  if(!is.null(.paramid)) arg.ls <- arg.ls[.paramid]
  names(arg.ls) <- NULL
  start <- proc.time()

  res.l <- parallel::mclapply(arg.ls, do.rep, .f=wrapWE(.f),
                              .reps=.reps, mc.cores=.mc.cores, .verbose=.verbose,
                              .eval=.eval, .rep.cores=1)

  end <- proc.time()
  res.l <- unlist(res.l, recursive=FALSE)

  err <- lapply(res.l, function(r){attr(r, "err")})
  err.id <-  which(unlist(lapply(err, function(x){!is.null(x)})))
  err.list <- err[err.id]
  names(err.list) <- err.id

  warn <- lapply(res.l, function(r){attr(r, "warn")})
  warn.id <- which(unlist(lapply(warn, function(x){!is.null(x)})))
  warn.list <- warn[warn.id]
  names(warn.list) <- warn.id

  class(res.l) <- c("gresults", class(res.l))
  attr(res.l, "time") <- end-start
  attr(res.l, "arg_names") <- names(arg_grid)
  attr(res.l, ".f") <- .f
  attr(res.l, "arg_grid") <- arg_grid
  attr(res.l, ".args") <- .args
  attr(res.l, "err") <- err.list
  attr(res.l, "warn") <- warn.list
  attr(res.l, ".reps") <- .reps
  return(res.l)
}

#' Evaluate a function repeatedly over arbitrary arguments
#'
#' This idiom is really useful to carry out simulations, which are essentially
#' repeated evaluations of a function over a grid of parameter values.
#'
#' @param .f function to be evaluated
#' @param ... Arguments passed to f
#' @param .reps the number of times the function should be evaluated
#' @param .rep.cores Apply repeates in parallel using mclapply
#' @param .eval If \code{TRUE} (default), evaluates \code{f}. If \code{FALSE}, does not evaluate \code{f}.
#' @param .verbose If \code{1} (default), prints a \code{.} with every completed condition.
#' @param .args optional list of (named) arguments to .f
#' If \code{2}, prints the arguments corresponding to the completed condition.
#' If \code{3}, prints the arguments and results of the completed condition.
#' @export
#' @importFrom parallel mclapply
#' @importFrom dplyr rbind_all as.tbl
#' @importFrom utils head
do.rep <- function(.f, ..., .reps, .verbose=1,.rep.cores=1, .eval=T, .args=NULL){
  if(.verbose %in% c(2,3) & .eval){cat(paste(names(...),"=", ...),fill=T)}
  if(.eval){
    res.l <- parallel::mclapply(1:(.reps),function(.rep, .f, ...){
      do.call(.f,c(.args, ...))}, .f=.f, ..., mc.cores=.rep.cores)
  } else {
    nothing <- function(...){c(NA)}
    res.l <- lapply(1:.reps, function(r, ...) do.call(nothing, ...), ...)
  }
  #res <- as.data.frame(do.call(rbind, res.l)); don't be tempted to do this!
  if(.verbose==1 & .eval){cat(".")}
  if(.verbose == 3 & .eval) { print(head(res.l))}
  if(.verbose > 1 & .eval) { cat("", fill=T) }
  return(res.l)
}

#' Wraps a function so that it catches and returns warnings and errors as attributes
#' @param fun function
#' @return \code{fun}
#' @export
wrapWE <- function(fun){
  function(...) {
    warn <- err <- NULL
    res <- withCallingHandlers(
      tryCatch(fun(...), error=function(e) {
        err <<- conditionMessage(e)
        c(NA)
      }), warning=function(w) {
        warn <<- append(warn, conditionMessage(w))
        invokeRestart("muffleWarning")
      })
    attr(res, "warn") <- warn
    attr(res, "err")  <- err
    res
  }
}

#' Errors from collected results
#' @param object from a call to \code{collect.gresults}
#' @export
err <- function(object){
  attr(object, "err")
}

#' Warnings from collected results
#' @param object from a call to \code{collect.gresults}
#' @export
warn <- function(object){
  attr(object, "warn")
}

is.error <- function(x){!is.null(attr(x, "err"))}
is.warn <- function(x){!is.null(attr(x, "warn"))}


