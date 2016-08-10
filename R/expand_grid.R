# expand_grid returning nested list
#' @export
expand_grid <- function(...){
  dots <- as.pairlist(...)
  rep.fac <- 1
  orep <- prod(sapply(dots, length))
  expand.id <- list()
  res <- list()
  for(i in seq_along(dots)){
    x <- dots[[i]]
    nx <- ifelse(is.data.frame(x), 1, length(x))
    orep <- orep / nx
    res[[i]] <- x[rep.int(rep.int(seq_len(nx), rep.int(rep.fac, nx)), orep)]
    rep.fac <- rep.fac * nx
  }
  names(res) <- names(dots)
  return(res)
}

split.list <- function(x, f, drop = F){

}



