## Applies f to argument node locally
#' @export
grid_apply <- function(.f, ..., .reps=1, .mc.cores=1, .verbose=1, .eval=T, .paramid=NULL){
  dots <- as.pairlist(...)
  arg.grid <- expand_grid(dots)
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
  attr(res.l, "f") <- .f
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
#' @param param.grid expanded grid of arguments
#' @param .reps number of reps
tidy.gresults <- function(x, param.grid, .reps){
  value <- as.data.frame(do.call(rbind, x))

  rep.grid <- x$grid[rep(1:nrow(param.grid),each=.reps), , drop=F]
  rep.grid$rep  <- rep(1:.reps, times=nrow(param.grid))

  rows.flag <- any(!sapply(sapply(x, nrow),is.null))
  gridl <- list()
  key2l <- list()

  for(i in 1:length(x)){
    # Add a second key if a data frame is returned
    if(rows.flag){
      reprow <- as.data.frame(x[[i]])
      key2l[[i]] <- rownames(reprow)
      nm <- nrow(reprow)
      gridl[[i]] <- rep.grid[rep(i, nm), ]
    } else {
      gridl[[i]] <- rep.grid[i, ]
    }
  }
  grid <- do.call(rbind, gridl)
  key2 <- unlist(key2l)
  grid$key2 <- key2

  wide <- cbind(grid, value)
  long <- tidyr::gather(wide,key,value,-(1:(ncol(wide)-ncol(value))))
  return(long)
}

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`


#' @export
print.gresults <- function(object){
  attributes(object) <- NULL
  print(object)
}
