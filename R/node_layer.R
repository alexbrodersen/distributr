# dgraph data: .layer
# layer data: .node
# node data: .f, .args, .id, .fname

# dgraph attributes: .control, .graph
# layer attributes: .id
# node attributes: names

#' Add layer to dgraph
#' @param ... nodes, separated by commas
#' @param .reduce if TRUE, the results of previous layers are expanded by nodes, otherwise reduced
#' @export
layer <- function(..., .reduce = FALSE){
  # the .dgraph will be listed first in all but the first one
  dots <- list(...)
  layer <- dots[sapply(dots, is.node)]

  stopifnot(sapply(layer, is.node))
  class(layer) <- "layer"

  if(is.dgraph(dots[[1]])){
    .dgraph <- dots[[1]]
    .id = max(sapply(.dgraph, function(l){attr(l, ".id")})) + 1
    .dgraph  <- add_layer(layer, .dgraph = .dgraph, .id= .id, .reduce = .reduce)
  } else {
    if(.reduce) { stop("can't reduce on first layer") }
    .dgraph <- layer_to_dgraph(layer) %>% control()
  }
  class(.dgraph) <- c(class(.dgraph), "dgraph")
  return(.dgraph)
}

#' Alias for \code{layer}
#' @describeIn layer alias for \code{layer}
#' @export
grid_map <- layer

#' @describeIn layer alias for \code{layer(..., .reduce=TRUE)}
#' @export
grid_reduce <- function(...){ layer(..., .reduce = TRUE) }


#' Apply a function to a grid of its parameters in a dgraph
#' @param .f function
#' @param ... arguments to \code{.f} given as vectors as in \code{grid_apply}
#' @param .dep which nodes does this one depnend on?
#' If NULL, depends on all nodes of previous layer.
#' @param .id custom id. if \code{NULL}, one will be assigned.
#' @export
node <- function(.f, ..., .dep=NULL, .id=NULL){
  .args <- list(...)
  .fname <- deparse(substitute(.f))
  out <- list(.f=.f, .args=.args, .dep=.dep, .id=.id, .fname=.fname)
  class(out) <- "node"
  return(out)
}

#' Control for dgrpah
#' @param .dgraph dgraph
#' @param .reps number of replications
#' @param .mc.cores number of cores
#' @param .tidy whether results are tidied
#' @param .backend backend to use. Currently only the default "sge" is defined
#' @param .cache which layers/nodes to cache. Currently only the default "all" is defined
#' @param .verbose verbosity in printing, can be 1, 2, or 3
#' @export
control <- function(.dgraph, .reps=1, .mc.cores = 1, .tidy=NULL,
                     .backend="sge", .cache="all", .verbose=1){
  .control <- list(.mc.cores = .mc.cores, .tidy=.tidy, .backend=.backend, .cache=.cache, .verbose=.verbose)
  attr(.dgraph, ".control")[names(.control)] <- .control
  return(.dgraph)
}

#' Checks if x is a dgraph, node or layer
#' @param x object
#' @export
is.dgraph <- function(x){ "dgraph" %in% class(x)}

#' @export
#' @describeIn is.dgraph Checks if x is a layer
is.layer <- function(x){ (class(x) == "layer") && (length(class(x)) == 1)}

#' @export
#' @describeIn is.dgraph Checks if x is a node
is.node <- function(x){ "node" %in% class(x)}



# lifts the first layer to dgraph (cannot reduce)
layer_to_dgraph <- function(.layer){
  .layer <- assign_node_ids(.layer, start = 0)
  attr(.layer, ".id") <- 1
  graph <- layer_to_graph(.layer, .reduce = FALSE, graph = NULL)
  .dgraph <- list(.layer)
  attr(.dgraph, ".graph") <- graph
  class(.dgraph) <- "dgraph"
  return(.dgraph)
}

# adds a layer to a dgraph
add_layer <- function(.layer, .dgraph, .id, .reduce){
  attr(.layer, ".id") <- .id
  g <- attr(.dgraph, ".graph")
  control <- attr(.dgraph, ".control")
  .layer <- assign_node_ids(.layer, start = max(g$node.id))
  .dgraph <- c(.dgraph, list(.layer)) # loses attributes??
  graph <- layer_to_graph(.layer, graph = g, .reduce = .reduce)
  attr(.dgraph, ".graph") <- graph
  attr(.dgraph, ".control") <- control
  return(.dgraph)
}


# Add layer to task graph, and update task graph if already exists
layer_to_graph <- function(l, graph = NULL, .reduce){
  node.ids <- layer_apply(l, select=".id")
  lid <- attr(l, ".id")
  n.nodes <- length(l)
  ntasks <- layer_apply(l, FUN=function(a){
   ifelse(length(a) == 0, 1, prod(sapply(a, length)))
  }, select = ".args")

  if(is.null(graph)){
    # root nodes
    dep <- node.ids
    tlow <- c(1, 1+ntasks)[-(n.nodes+1)]
    tup <- cumsum(ntasks)
    graph <- data.frame(layer = rep(attr(l, ".id"), n.nodes), dep=dep, node.id=node.ids,
                        node.pos = node.ids, ntasks = ntasks, tlow=tlow, tup=tup)

  } else {
    sub_graph <- subset(graph, layer == lid - 1)
    start <- max(sub_graph[, "tup"])
    dep <- sub_graph$node.pos
    pos.start <- max(sub_graph$node.pos) + 1

    # basically, don't grid over all dependencies.
    # setting this to 0 is a flag for do.one to load previous layer instead of a particular node.
    if(.reduce) dep <- 0

    # create a row for each dependency
    dep.rows <- expand.grid(layer = lid, dep=dep, node.id=node.ids)

    dep.rows$node.pos <- pos.start : (pos.start + nrow(dep.rows) - 1)
    if(.reduce) {
      dep.rows$ntasks <- 1
    } else {
      dep.rows$ntasks <- rep(ntasks, each = length(dep)) * rep(sub_graph$ntasks, times = n.nodes)
    }
    dep.rows$tlow <- head(c(start + 1, start + 1 + cumsum(dep.rows$ntasks)), -1)
    dep.rows$tup <- cumsum(dep.rows$ntasks) + start
    graph <- rbind(graph, dep.rows)
  }
  return(graph)
}

#' Apply function to all nodes in a layer
#' @param l layer
#' @param select name of object in layer to select. Default is a node, given by ".id"
#' @param FUN function to apply
#' @export
layer_apply <- function(l, select=".id", FUN=I){
  sapply(l, function(x){FUN(x[[select]])})
}

assign_node_ids <- function(e, start=0){
  for(i in which(sapply(e, is.node))){
    if(is.null(e[[i]]$.id)) e[[i]]$.id <- start + i
  }
  return(e)
}

#' Run all computations in a graph repeatedly
#' @param .dgraph dgraph
#' @param .reps number of replications to run
#' @export
reps <- function(.dgraph, .reps){
  attr(.dgraph, ".control")[".reps"] <- .reps
  return(.dgraph)
}

#' Get node from a dgraph by id
#' @param dgraph dgraph
#' @param id node id
#' @export
get_node <- function(dgraph, id){

  ids <- lapply(dgraph, function(l){
     layer_apply(l, select = ".id")
  })

  # This is not fast, but hopefully clearish
  which_layer <- sapply(ids, function(nid){any(nid == id)}) %>% which
  pos_in_layer <- which(ids[[which_layer]] == id)

  dgraph[[which_layer]][[pos_in_layer]]
}

# # this is just a sketch right now, it's going to be complicated
# run_node <- function(dgraph, id){
#   node <- get_node(dgraph, id)
#   graph <- attr(dgraph, ".graph")
#
#   # compute/load any dependencies
#
#   # compute args
#   args <- expand.grid(node$.args)
#
#   # run grid_apply on args
#
#   args <- purrr::flatten(list(prev_res, args[-1]))
#   names(args)[1] <- names(formals(node$.f))[1]
#
#   res.l <- grid_apply(.f = node$.f, args,
#                       .reps = 1, .mc.cores = control$.mc.cores, .verbose = control$.verbose)
# }

#' Return the parameter graph implied by the dgraph grid
#' @param dgraph dgraph
#' @param layer.id id of the layer to expand. If NULL, expands all layers.
#' @export
expand_grid_dgraph <- function(dgraph, layer.id=NULL){
  # get the arguments from terminal node
  graph <- attr(dgraph, ".graph")

  terminal_layer <- ifelse(is.null(layer.id), max(graph$layer), layer.id)

  get_args_subgraph <- function(dgraph, graph, node.pos){
    sub_graph <- graph[node.pos, ]
    node <- sub_graph$node.id
    args <- get_node(dgraph, node)$.args

    if(sub_graph$node.pos == sub_graph$dep){
      # root node
      return(args)
    }
    # get the arg list of parent
    parent_args <- get_args_subgraph(dgraph, graph, node.pos = sub_graph$dep)

    args <- c(args, parent_args)
    return(args)
  }

  # expand arguments of the parents of all terminal nodes (subgraph)
  expanded_args.l <- list()
  nodes <- subset(graph, layer == terminal_layer)$node.pos
  for(i in seq_along(nodes)){
    expanded_args.l[[i]] <- do.call(expand.grid,
                                    get_args_subgraph(dgraph, graph, node.pos = nodes[i]))
  }
  res <- dplyr::bind_rows(expanded_args.l)
  return(res)
}




