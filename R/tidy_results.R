
#' Tidy results from grid_apply or dgraph
#'
#' Returns \code{key=value} pairs obtained from applying a \code{f} to a grid
#' of parameters using \code{grid_apply}, merging with the grid of parameters.
#' @param x results from \code{grid_apply} or \code{collect}
#' @param ... additional arguments passed to either \code{tidy.gresults} or
#' \code{tidy.dgraph}
#' @details Tidying depends on the form of the results returned from applying a
#' function \code{f} to a grid of parameters. If the results are a data frame,
#' the row and column names are mapped to two keys. If the results are a vector,
#' the names of the elements are mapped to a single key. If the results are unnamed,
#' names are assigned as in \code{as.data.frame}. See \code{\link{stack_list}} for
#' further details.
#' @return Returns non-error results as a \code{data.frame} in long form with
#'  the following columns:
#' \item{...}{Columns corresponding to grid of parameters given in
#' \code{expand.grid(...)}}
#' \item{\code{rep}}{the replication number}
#' \item{\code{key2}}{the rowname(s) of the returned data frame of \code{f} if present}
#' \item{\code{key}}{the colname(s) of the returned data frame of \code{f}} or
#' the names of the elements of a vector returned from \code{f}, or assigned names
#' \item{\code{value}}{the value of \code{f} at a set of parameters}
#' @seealso \code{\link{stack_list}}
#' @export
tidy <- function(x, ...){
  UseMethod("tidy")
}


#' @param arg_grid argument grid; if NULL (default) looks for arg_grid
#'  as an attribute to \code{x}
#' @param stack if \code{TRUE} (default) stack results with \code{stack_list}. Otherwise, \code{bind_rows} is used.
#' @param .reps scalar or vector of completed replications for each job (usually given via \code{collect})
#' @imporFrom dplyr bind_cols as_data_frame
#' @export
#' @describeIn tidy Tidy an object from \code{grid_apply} or \code{collect}
tidy.gresults <- function(x, arg_grid=NULL, stack=TRUE, .reps=NULL, ...){
  if(is.null(arg_grid)){
    arg_grid <- attr(x, "arg_grid")
    if(is.null(arg_grid)) stop("can't tidy, no argument grid")
  }
  if(is.null(.reps)) .reps <- attr(x, ".reps")
  if(length(.reps) == 1){
    # from grid_apply
    rep_grid <- arg_grid[rep(1:nrow(arg_grid), each=.reps), , drop=F]
    #if(nrow(rep_grid) != length(x)) stop("number of replications isn't correct, try setting .reps=NULL")
    rep_grid$.rep  <- rep(1:.reps, times=nrow(arg_grid))
  } else {
    # completed replications, from collect
    #if(length(x) != length(.reps)) stop("length(reps) should be 1 or length(x)")
    rep_grid <- arg_grid[rep(1:nrow(arg_grid), times=.reps), , drop=F]
    rep_grid$.rep <- unlist(lapply(.reps, seq_len))
  }

  # Stack results, adding keys according to names of elements, colnames, and rownames.
  if(stack){
    values <- stack_list(x)

    # Expand rows by number of keys
    if(is.data.frame(x[[1]])){
      nkeys <- sapply(x, function(xi){prod(dim(xi))})
    } else if(is.list(x[[1]])){
      nkeys <- sapply(x, function(xi){sum(lengths(xi))})
    } else {
      nkeys <- sapply(x, length)
    }
  } else {
    nkeys <- sapply(x, nrow)
    values <- bind_rows(x)
    #rep_grid$.rep <- NULL # I don't know, I'm guessing you probably don't want this?
  }

  value_grid <- rep_grid[rep(1:nrow(rep_grid), times = nkeys), ]

  rownames(value_grid) <- NULL
  res <- dplyr::bind_cols(dplyr::as_data_frame(value_grid), values)

  new.attr.names <- setdiff(names(attributes(x)), names(attributes(res)))
  attributes(res)[new.attr.names] <- attributes(x)[new.attr.names]
  attr(res, "class") <- c("gresults", class(res))
  return(res)
}

#' Tidying dgraph results
#'
#' @param x list of results
#' @param arg_grid argument grid
#' @param dir directory
#' @param layer.id index of the layer to tidy. If \code{NULL} tidies last layer.
#' @param ... unused
#' @export
tidy.dgraph <- function(x, arg_grid = NULL, dir=getwd(), layer.id = NULL, ...){
  # flatten because list of gresults, want just one list with arg_grid and reps as attributes
  res <- purrr::flatten(x)

  if(is.null(arg_grid)){
    arg_grid <- attr(x, "arg_grid") # try to grab the arg grid from the results
  }
  attributes(res) <- attributes(x)
  tidy.gresults(res, arg_grid = arg_grid)
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
#'@importFrom dplyr data_frame
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

#globalVariables(".")
