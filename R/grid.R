
#' @export
"+.grid" <- function(e1, e2){
  add_grid(e1, e2)
}

# takes a function f, and lifts it so that it can be applied to a grid of the given parameters
# but delay evaluation
#' @export
grid <- function(.f, ..., .level=0, .dep=NULL){
  grid <- expand.grid(...)
  name <- deparse(substitute(.f))
  out <- list(.f=.f, grid=grid, .level=.level, .dep=.dep, .name=name)
  class(out) <- "grid"
  return(out)
}

#' @export
isopt <- function(x){ any(grepl("goption", class(x)))}
#' @export
isgrid <- function(x){ (class(x) == "grid") && (length(class(x)) == 1)}
#' @export
isgraph <- function(x){ class(x) == "ggraph"}

#' @export
add_grid <- function(e1, e2){
  if(isgraph(e1) && isgrid(e2)){
    o <- c(e1, e2)
    class(o) <- "ggraph"
  } else if(isgrid(e1) && isgrid(e2)){
    o <- list(e1, e2)
    class(o) <- "ggraph"
  } else if(isopt(e2)){
    e1$options <- c(e1$options, e2)
    o <- e1
  } else {
    print("cant add")
  }
  return(o)
}

#' @export
reps <- function(n){
  o <- list(n=n)
  class(o) <- c("grid", "goption")
  return(o)
}

#' @export
tidy <- function(.level){
  o <- list(tidy=.level)
  class(o) <- c("grid", "goption")
  return(o)
}

# run graph locally in parallel across cores
#' @export
glocal <- function(.mc.cores=1){
  o <- list(env="local", .mc.cores=.mc.cores)
  class(o) <- c("grid", "goption")
  return(o)
}

## Applies f to argument grid locally
#' @export
gapply.local <- function(f, ..., .reps=1, .mc.cores=1, .verbose=1, .eval=T){
  grid <- expand.grid(...)
  param.ls <- split(grid, 1:nrow(grid))
  names(param.ls) <- NULL
  start <- proc.time()
  res.l <- parallel::mclapply(param.ls, do.rep, f=wrapWE(f),
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
  attr(res.l, "arg.names") <- colnames(grid)
  attr(res.l, "f") <- f
  attr(res.l, "grid") <- grid
  attr(res.l, "err") <- err.list
  attr(res.l, "warn") <- warn.list
  attr(res.l, ".reps") <- .reps
  return()
}

#' @export
tidy.gresults <- function(res.l){
  value <- as.data.frame(do.call(rbind, res.l))

  rep.grid <- param.grid[rep(1:nrow(param.grid),each=.reps), , drop=F]
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
# run.ggraph <- function(object){
#   if(object$env == "local"){
#     fs <- which(sapply(o, isgrid))
#     levels <- sapply(o[fs], function(g){g$.level})+1
#     run.order <- order(levels)
#     for(i in levels)){
#
#     }
#   }
#
# }



