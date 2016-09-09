
#' Tidy x
#' @export
tidy <- function(x, ...){
  UseMethod("tidy")
}

#' Tidy an object from grid_apply
#' @export
tidy.gresults <- function(x, arg_grid=NULL, .reps=NULL){
  if(is.null(arg_grid)){ arg_grid <- attr(x, "arg.grid")}
  if(is.null(.reps)){ .reps = attr(x, ".reps")}

  rep_grid <- arg_grid[rep(1:nrow(arg_grid),each=.reps), , drop=F]
  rep_grid$rep  <- rep(1:.reps, times=nrow(arg_grid))

  # Stack results, adding keys according to names of elements, colnames, and rownames.
  values <- stack_list(x)

  # Expand rows by number of keys
  if(is.data.frame(x[[1]])){
    nkeys <- sapply(x, function(xi){prod(dim(xi))})
  } else if(is.list(x[[1]])){
    nkeys <- sapply(x, function(xi){sum(lengths(xi))})
  } else {
    nkeys <- sapply(x, length)
  }

  value_grid <- rep_grid[rep(1:nrow(rep_grid), times = nkeys), ]

  rownames(value_grid) <- NULL
  res <- dplyr::as_data_frame(value_grid) %>% dplyr::bind_cols(., values)

  new.attr.names <- setdiff(names(attributes(x)), names(attributes(res)))
  attributes(res)[new.attr.names] <- attributes(x)[new.attr.names]
  attr(res, "class") <- c("gresults", class(res))
  return(res)
}


#' Stacks a list of vectors, lists, or data frames
#'
#' Stacks a list of vectors, lists, or data frames into a tibble with \code{key} and \code{value} columns,
#' us
#'
#' @param  xl list of vectors, lists, or data frames
#' @return tibble with \code{key}, and \code{value} as columns; \code{key2} if list of data frames. \code{key} and \code{key2} are not factors.
#' @export
#' @details
#'  Stacks a vector or list into \code{key} and \code{value} columns, where \code{key} takes the names of the elements,
#'  if the names are null, assigns names.
#'
#' Stacks a list of vectors into \code{key} and \code{value} columns, where \code{key} takes the names of the elements,
#'  if the names are null, assigns names.
#'
#' Stacks a data frame or list of vectors of the same length into \code{key}, \code{key2}, and \code{value} columns,
#'  where \code{key} and \code{key2} are the column and row names of the first element of \code{xl}. If names are null, assigns names.
#'
stack_list <- function(xl){
  x <- xl[[1]]
  if(is.data.frame(x)){
    same_dimensions <- lapply(xl, dim) %>%
      sapply(., function(dims){ identical(dims, dim(x))}) %>% all

    if(same_dimensions){
      value <- data_frame(key = rep(rep(colnames(x), each = nrow(x)), times = length(xl)),
                          key2 = rep(rownames(x), times = length(xl) * ncol(x)),
                          value = unlist(xl, use.names = F))
    } else {
      value <- lapply(xl, stack_x) %>% bind_rows
    }

  } else if(is.list(x)){
    same_dimensions <- lapply(xl, lengths) %>%
      sapply(., function(dims){ lengths(x) == length(x[[1]]) }) %>% all

    if(same_dimensions){
      x <- as.data.frame(x)
      value <- data_frame(key = rep(rep(colnames(x), each = nrow(x)), times = length(xl)),
                          key2 = rep(rownames(x), times = length(xl) * ncol(x)),
                          value = unlist(xl, use.names = F))
    } else {
      value <- lapply(xl, stack_x) %>% bind_rows
    }

  } else {
    same_dimensions <- all(sapply(xl, length) == length(x))

    if(same_dimensions){
      if(is.null(names(x))){
        value <- data_frame(key = rep(paste0("V", seq_along(x)), times = length(xl)),
                            value = unlist(xl, use.names = F))
      } else {
        value <- data_frame(key = rep(names(x), times = length(xl)),
                            value = unlist(xl, use.names = F))
      }
    } else{
      value <- lapply(xl, stack_x) %>% bind_rows
    }
  }
  return(value)
}


#' Stack a simple vector or data frame into key and value columns
#'
#' Stacks a vector or list into \code{key} and \code{value} columns, where key takes the names of the elements, or
#' assigns names if null.
#'
#' Stacks a list of vectors into \code{key} and \code{value} columns, where key names its elements automatically
#'
#' Stacks a data frame into \code{key} (key2) and \code{value} columns, where key and key2 are the column
#' and row names of x.
#'
#' @param x a vector or data frame
stack_x <- function(x){
  if(is.data.frame(x)){
      data_frame(key = rep(colnames(x), each = nrow(x)),
                 key2 = rep(rownames(x), times = ncol(x)),
                 value = unlist(x, use.names = F))
  } else {
    if(is.list(x)) x <- unlist(x)

    if(is.null(names(x))){
      data_frame(key = paste0("V", seq_along(x)), value = x)
    } else {
      data_frame(key = names(x), value = x)
    }
  }
}
