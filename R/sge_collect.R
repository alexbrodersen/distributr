#' Collect completed results files from SGE
#'
#' Provides arguments to return a sample of the results files of a given size,
#' or to return results from files matching a regular expression.
#' @param dir directory
#' @param filter a quoted string or formula filtering jobs in \code{arg_grid} as in \code{dplyr::filter}.
#' Results for jobs matching filter are returned (requires \code{dplyr})
#' If \code{NULL} (default), all results are returned.
#' @param regex regular expression  matching files in \code{results/}.
#' If \code{NULL} (default), all results are returned.
#' @param sample the number of files in \code{results/} to sample from.
#' If \code{NULL} (default), all results are returned.
#' @details \code{filter, regex} and \code{sample} are applied to the available results in order.
#' For example, results are filtered first, a regex is applied, then a sample is taken.
#' @export
#' @describeIn collect collect results from \code{grid_apply, gapply}
collect <- function(dir=getwd(), filter=NULL, regex=NULL, sample=NULL){
  dir <- paste0(dir, "/")
  arg_grid <- readRDS(paste0(dir, "arg_grid.Rdata"))

  rdir <- paste0(dir, "results/")
  fns <- paste0(rdir, dir(rdir))
  ids <- as.numeric(gsub(".Rdata", "", dir(rdir)))
  conds.files <- fns[order(ids)]

  if(!is.null(filter)){
    if (!requireNamespace("dplyr", quietly = TRUE)) {
      stop("dplyr needed to filter. Please install it.",
           call. = FALSE)
    }
    grid_filter <- dplyr::filter_(arg_grid, .dots=filter)
    conds.files <- conds.files[ids %in% grid_filter$.sge_id]
  }
  if(!is.null(regex)){
    conds.files <- grep(conds.files, pattern = regex, value=TRUE)
  }
  if(!is.null(sample)){
    conds.files <- conds.files[sample(1:length(conds.files), size = sample, replace = FALSE)]
  }
  cond.l <- list()           # list of the results from each condition
  #pb <- txtProgressBar(min=0, max=length(conds.files), style=3)
  for(i in 1:length(conds.files)){
    fn <- paste0(conds.files[i])
    res.l <- readRDS(fn)
    cond.l[[i]] <- res.l
    #setTxtProgressBar(pb, i)
  }
  reps <- lengths(cond.l)

  cond.l <- unlist(cond.l, recursive=F)

  completed.idx <- as.numeric(gsub(".Rdata", "", basename(conds.files))) # completed conditions
  completed.grid <- arg_grid[completed.idx,]

  err <- lapply(cond.l, function(r){attr(r, "err")})
  err.id <-  which(unlist(lapply(err, function(x){!is.null(x)})))
  err.list <- err[err.id]
  names(err.list) <- err.id

  warn <- lapply(cond.l, function(r){attr(r, "warn")})
  warn.id <- which(unlist(lapply(warn, function(x){!is.null(x)})))
  warn.list <- warn[warn.id]
  names(warn.list) <- warn.id

  times <- lapply(cond.l, function(r){attr(r, "time")})

  res <- cond.l
  class(res) <- c("gapply", class(res))
  attr(res, "arg_grid") <- completed.grid
  attr(res, ".reps") <- reps
  attr(res, "err") <- err.list
  attr(res, "warn") <- warn.list

  return(res)
}

