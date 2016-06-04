#' Repeatedly apply a function over a grid of values in parallel
#'
#' gapply (grid apply) applies a function to a grid of it's parameters in parallel, optionally for a given number of replications
#'
#' @param f function to be evaluated. The function must return a (named) value or (named) vector of values.
#' @param ... named arguments to \code{f} in the form \code{key=c(value1,value2, ...)} etc. 
#' A grid of parameter values will be generated from values given to each named argument, as \code{expand.grid(...)}
#' @param .reps times the function should be evaluated
#' @param .mc.cores attempts to split function evaluations over given number of cores
#' @param .verbose If \code{1} (default), prints a \code{.} with every completed condition. 
#' If \code{2}, prints the arguments corresponding to the completed condition. 
#' If \code{3}, prints the arguments and results of the completed condition.
#' @param .eval If \code{TRUE} (default), evaluates \code{f}. If \code{FALSE}, does not evaluate \code{f} and returns \code{NA} for \code{value}.
#' @return Returns non-error results as a \code{data.frame} in long form with the following columns:
#' \item{...}{Columns corresponding to grid of parameters given in \code{expand.grid(...)}}
#' \item{\code{rep}}{the replication number}
#' \item{\code{key}}{the name(s) of the return value(s) of \code{f}}
#' \item{\code{value}}{the value of \code{f} at a set of parameters, if \code{.eval = FALSE}, returns \code{NA}}
#' Errors are captured using \code{try}, converted to character, and available using \code{attr(object, "err")}
#' @details 
#' The attributes of the object include \code{grid} (the grid of parameter values), \code{time} (elapsed time), and \code{err} (list of errors).
#' 
#' If the values returned by \code{f} are not named, they will be named according to the rules of \code{as.data.frame}, typically \code{V1, V2, ...}. 
#' 
#' The function application to each combination of meta-parameters (not replications) are distributed in parallel via \code{mclapply} and will not work in Windows. 
#' @examples
#' do.one <- function(a=1,b=2){c(sum=a+b,sub=a-b)}
#' gapply(do.one, a=1:4,b=2:3, .reps=5)
#' @export
#' @importFrom tidyr gather
#' @importFrom parallel mclapply
#' @importFrom dplyr rbind_all
gapply <- function(f, ..., .reps=1, .mc.cores=1, .verbose=1, .eval=T){
  param.grid <- expand.grid(...)
  param.ls <- split(param.grid, 1:nrow(param.grid))
  names(param.ls) <- NULL
  start <- proc.time()
  res.l <- parallel::mclapply(param.ls, do.rep, f=wrapWE(f), 
                            .reps=.reps, mc.cores=.mc.cores, .verbose=.verbose, 
                            .eval=.eval, .rep.cores=1)
  end <- proc.time()
  

  # take off one level of nesting and make sure all elements are data.frame
  res.l <- unlist(res.l, recursive=FALSE)
  rc <- length(res.l) # reps * conditions
  
  err <- lapply(res.l, function(r){attr(r, "err")})
  err.id <-  which(unlist(lapply(err, function(x){!is.null(x)})))
  err.list <- err[err.id]
  names(err.list) <- err.id
  
  warn <- lapply(res.l, function(r){attr(r, "warn")})
  warn.id <- which(unlist(lapply(warn, function(x){!is.null(x)})))
  warn.list <- warn[warn.id]
  names(warn.list) <- warn.id
  
  value <- as.data.frame(do.call(rbind, res.l)) # automatic naming of unnamed returns to V1,V2, etc

  rep.grid <- param.grid[rep(1:nrow(param.grid),each=.reps), , drop=F]
  rep.grid$rep  <- rep(1:.reps, times=nrow(param.grid))
  gridl <- list()
  methodl <- list()
  for(i in 1:length(res.l)){
    reprow <- as.data.frame(res.l[[i]])
    nm <- nrow(reprow)
    methodl[[i]] <- rownames(reprow)
    gridl[[i]] <- rep.grid[rep(i, nm), ]
  }
  mgrid <- do.call(rbind, gridl)
  method <- unlist(methodl)
  mgrid$method <- method

  wide <- cbind(mgrid, value)
  
  long <- tidyr::gather(wide,key,value,-(1:(ncol(param.grid)+2)))
  
  class(long) <- c("gapply", class(long))
  attr(long, "time") <- end-start
  attr(long, "arg.names") <- colnames(param.grid)
  attr(long, "f") <- f
  attr(long, "param.grid") <- param.grid
  attr(long, "err") <- err.list
  attr(long, "warn") <- warn.list
  attr(long, ".reps") <- .reps
  return(long)
}

#' Evaluate a function repeatedly over arbitrary arguments
#'
#' This idiom is really useful to carry out simulations, which are essentially
#' repeated evaluations of a function over a grid of parameter values.
#'
#' @param f function to be evaluated
#' @param ... Arguments passed to f
#' @param .reps the number of times the function should be evaluated
#' @param .rep.cores Apply repeates in parallel using mclapply
#' @param .eval If \code{TRUE} (default), evaluates \code{f}. If \code{FALSE}, does not evaluate \code{f}.
#' @param .verbose If \code{1} (default), prints a \code{.} with every completed condition. 
#' If \code{2}, prints the arguments corresponding to the completed condition. 
#' If \code{3}, prints the arguments and results of the completed condition.
#' @export
#' @importFrom parallel mclapply
#' @importFrom dplyr rbind_all as.tbl


do.rep <- function(f,..., .reps,.verbose=1,.rep.cores=1, .eval=T){
  if(.verbose %in% c(2,3) & .eval){cat(paste(names(...),"=", ...),fill=T)}
  if(.eval){
    res.l <- parallel::mclapply(1:(.reps),function(r, f, ...){ 
      do.call(f,...)}, f=f, ..., mc.cores=.rep.cores)
  } else {
    nothing <- function(...){c(NA)}
    res.l <- lapply(1:.reps, function(r, ...) do.call(nothing, ...), ...)
  }
  #res <- as.data.frame(do.call(rbind, res.l))
  if(.verbose==1 & .eval){cat(".")}
  if(.verbose == 3 & .eval) { print(head(res.l))}
  if(.verbose > 1 & .eval) { cat("", fill=T) }
  return(res.l)
}

#' @export
wrapWE <- function(fun){
  function(...) {
    warn <- err <- NULL
    res <- withCallingHandlers(
      tryCatch(fun(...), error=function(e) {
        err <<- conditionMessage(e)
        NA
      }), warning=function(w) {
        warn <<- append(warn, conditionMessage(w))
        invokeRestart("muffleWarning")
      })
    attr(res, "warn") <- warn
    attr(res, "err")  <- err
    res
  }
}


is.error <- function(x){!is.null(attr(x, "err"))}
is.warn <- function(x){!is.null(attr(x, "warn"))}

expand.reps.grid <- function(param.grid, .reps){
  rep.grid <- param.grid[rep(1:nrow(param.grid),each=.reps), , drop=F]
  rep.grid$rep  <- rep(1:.reps, times=nrow(param.grid))
  return(rep.grid)
}
