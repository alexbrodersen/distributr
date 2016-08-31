## Applies f to argument node
#' @export
#' @importFrom purrr transpose
#' @importFrom parallel mclapply
grid_apply <- function(.f, ..., .reps=1, .mc.cores=1, .verbose=1, .eval=T, .paramid=NULL){
  arg.grid <- expand.grid(...)
  arg.ls <- purrr::transpose(arg.grid)

  if(!is.null(.paramid)) arg.ls <- arg.ls[.paramid]
  names(arg.ls) <- NULL
  start <- proc.time()

  res.l <- parallel::mclapply(arg.ls, do.rep, .f=wrapWE(.f),
                              .reps=.reps, mc.cores=.mc.cores, .verbose=.verbose,
                              .eval=.eval, .rep.cores=1)

  end <- proc.time()
  res.l <- unlist(res.l, recursive=FALSE)
  cat("", fill=T)

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
  attr(res.l, "arg.names") <- names(arg.grid)
  attr(res.l, ".f") <- .f
  attr(res.l, "arg.grid") <- arg.grid
  attr(res.l, "err") <- err.list
  attr(res.l, "warn") <- warn.list
  attr(res.l, ".reps") <- .reps
  return(res.l)
}

#' Tidy output from grid_apply or collect.dgraph
#'
#' @export
#' @param x list of results from grid_apply or collect.dgraph
#' @param arg.grid expanded grid of arguments
#' @param .reps number of reps
tidy.gresults <- function(x, arg.grid=NULL, .reps=NULL){

  if(is.null(arg.grid)){ arg.grid <- attr(x, "arg.grid")}
  if(is.null(.reps)){ .reps = attr(x, ".reps")}

  rep.grid <- arg.grid[rep(1:nrow(arg.grid),each=.reps), , drop=F]
  rep.grid$rep  <- rep(1:.reps, times=nrow(arg.grid))

  # Stack values by key
  val <- lapply(x, stackx)
  nkeys <- sapply(val, nrow)

  # Expand rows by number of keys
  val.grid <- rep.grid[rep(1:nrow(rep.grid), nkeys), ]
  rownames(val.grid) <- NULL
  res <- dplyr::as_data_frame(val.grid) %>% dplyr::bind_cols(., dplyr::bind_rows(val))

  new.attr.names <- setdiff(names(attributes(x)), names(attributes(res)))
  attributes(res)[new.attr.names] <- attributes(x)[new.attr.names]
  attr(res, "class") <- c("gresults", class(res))
  return(res)
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
do.rep <- function(.f, ..., .reps, .verbose=1,.rep.cores=1, .eval=T, .args=NULL){
  if(.verbose %in% c(2,3) & .eval){cat(paste(names(...),"=", ...),fill=T)}
  if(.eval){
    res.l <- parallel::mclapply(1:(.reps),function(.rep, .f, ...){
      do.call(.f,c(.args, ...))}, .f=.f, ..., mc.cores=.rep.cores)
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
        c(err = NA)
      }), warning=function(w) {
        warn <<- append(warn, conditionMessage(w))
        invokeRestart("muffleWarning")
      })
    attr(res, "warn") <- warn
    attr(res, "err")  <- err
    res
  }
}

#' @export
err <- function(object){
  attr(object, "err")
}
#' @export
warn <- function(object){
  attr(object, "warn")
}

is.error <- function(x){!is.null(attr(x, "err"))}
is.warn <- function(x){!is.null(attr(x, "warn"))}

#' takes a variety of inputs, and stacks into a consistent format (for me?)
#'  x is vector                    : key with auto-names
#'  x is vetor with named keys     : key with given names
#'  x is matrix                    : key with auto-names for a given number of rows
#'  x is df with nrow = 1          : key with colnames
#'  x is df with nrow > 1          : key with colnames, key2 with rownames
stackx <- function(x){
  if(is.data.frame(x)){
    if(nrow(x) > 1){
      x %>% stack %>% dplyr::transmute(key2 = rep(rownames(x), times = ncol(x)),
                                key = as.character(ind),
                                value = values)
    } else {
      x %>% stack %>% dplyr::transmute(key = as.character(ind), value = values)
    }
  } else {
    t(x) %>% as.data.frame %>% stack %>% transmute(key = as.character(ind), value = values)
  }
}

#' @importFrom magrittr %>%
magrittr::`%>%`

