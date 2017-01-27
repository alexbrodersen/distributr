#' Repeatedly apply a function over a grid of values in parallel
#'
#' gapply (grid apply) applies a function to a grid of it's parameters in parallel, optionally for a given number of replications
#'
#' @param .f function to be evaluated. The function must return a (named) value or (named) vector of values.
#' @param ... named arguments to \code{f} in the form \code{key=c(value1,value2, ...)} etc.
#' A grid of parameter values will be generated from values given to each named argument, as \code{expand.grid(...)}
#' @param .reps times the function should be evaluated
#' @param .args list of extra arguments to \code{.f} not included in the argument grid.
#' @param .mc.cores attempts to split function evaluations over given number of cores
#' @param .verbose If \code{1} (default), prints a \code{.} with every completed condition.
#' If \code{2}, prints the arguments corresponding to the completed condition.
#' If \code{3}, prints the arguments and results of the completed condition.
#' @param .eval If \code{TRUE} (default), evaluates \code{f}. If \code{FALSE}, does not evaluate \code{f} and returns \code{NA} for \code{value}.
#' @param .stack whether results should be stacked (see \code{tidy})
#' @return Returns results as a \code{data.frame} in long form with the following columns:
#' \item{...}{Columns corresponding to grid of parameters given in \code{expand.grid(...)}}
#' \item{\code{.rep}}{the replication number}
#' \item{\code{value}}{the value of \code{f} at a set of parameters, if \code{.eval = FALSE}, returns \code{NA}}
#' Errors are captured using \code{try}, converted to character, and available
#' using \code{attr(object, "err")}
#' @details
#' The attributes of the object include \code{grid} (the grid of parameter values),
#'  \code{time} (elapsed time), and \code{err} (list of errors).
#'
#' The function application to each combination of meta-parameters (not replications)
#' are distributed in parallel via \code{mclapply} and will not work in Windows.
#'
#' @examples
#' do.one <- function(a=1,b=2){c(sum=a+b,sub=a-b)}
#' gapply(do.one, a=1:4,b=2:3, .reps=5)
#' @export
#' @importFrom parallel mclapply
gapply <- function(.f, ..., .reps=1, .args=NULL, .mc.cores=1, .verbose=1, .eval=T, .stack=FALSE){
  grid_apply(.f, ..., .reps = .reps, .args=.args, .mc.cores = .mc.cores,
             .verbose = .verbose, .eval = .eval) %>%
    tidy(., stack=.stack)
}
