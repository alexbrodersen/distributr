## Applies f to argument node locally
#' @export
gapply.local <- function(.f, ..., .reps=1, .param.cores=1, .rep.cores=1, .verbose=1, .eval=T, .args=NULL, .paramid=NULL){
  grid <- expand.grid(...)
  param.ls <- split(grid, 1:nrow(grid))
  if(!is.null(.paramid)) param.ls <- param.ls[.paramid]
  names(param.ls) <- NULL
  start <- proc.time()
  stopifnot(length(.args) == .reps)
  onerep <- function(r, .args, ...){ parallel::mclapply(param.ls, do.rep, .args = .args[r], ..., mc.cores = .param.cores)}

  res.l <- parallel::mclapply(1:.reps, onerep, .args = .args, .f=wrapWE(.f),
                              .reps=.reps, .verbose=.verbose, .param.cores = .param.cores,
                              .eval=.eval, mc.cores = .rep.cores)

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
  attr(res.l, "arg.names") <- colnames(grid)
  attr(res.l, "f") <- .f
  attr(res.l, "grid") <- grid
  attr(res.l, "err") <- err.list
  attr(res.l, "warn") <- warn.list
  attr(res.l, ".reps") <- .reps
  return(res.l)
}

#' @export
tidy.gresults <- function(res.l){
  value <- as.data.frame(do.call(rbind, res.l))

  rep.grid <- res.l$grid[rep(1:nrow(param.grid),each=.reps), , drop=F]
  rep.grid$rep  <- rep(1:.reps, times=nrow(param.grid))

  rows.flag <- any(!sapply(sapply(res.l, nrow),is.null))
  gridl <- list()
  key2l <- list()

  for(i in 1:length(res.l)){
    # Add a second key if a data frame is returned
    if(rows.flag){
      reprow <- as.data.frame(res.l[[i]])
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



#' @export
print.gresults <- function(object){
  attributes(object) <- NULL
  print(object)
}
