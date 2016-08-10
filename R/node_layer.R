# dgraph data: .layer
# layer data: .node
# node data: .f, .args, .id, .fname

# dgraph attributes: .dcontrol, .graph
# layer attributes: .id
# node attributes: names

#' Add layer to dgraph
#' @param ... nodes, separated by commas
#' @export
layer <- function(...){
  # the .dgraph will be listed first in all but the first one
  dots <- list(...)
  layer <- dots[sapply(dots, is.node)]

  stopifnot(sapply(layer, is.node))
  class(layer) <- "layer"

  if(is.dgraph(dots[[1]])){
    .dgraph <- dots[[1]]
    .id = max(sapply(.dgraph, function(l){attr(l, ".id")})) + 1
    .dgraph  <- add_layer(layer, .dgraph = .dgraph, .id= .id)
  } else {
    .dgraph <- layer_to_dgraph(layer) %>% dcontrol()
  }
  return(.dgraph)
}

# takes a function f, and lifts it so that it can be applied to a node of the given parameters
# but delay evaluation
#' @export
node <- function(.f, ..., .dep=NULL, .id=NULL){
  .args <- list(...)
  .fname <- deparse(substitute(.f))
  out <- list(.f=.f, .args=.args, .dep=.dep, .id=.id, .fname=.fname)
  class(out) <- "node"
  return(out)
}

#' @export
dcontrol <- function(.dgraph, reps=1, mc.cores = 1, tidy=NULL,
                     backend="sge", cache="all", verbose=1){
  .dcontrol <- list(mc.cores = mc.cores, tidy=tidy, backend=backend, cache=cache, verbose=verbose)
  attr(.dgraph, ".dcontrol")[names(.dcontrol)] <- .dcontrol
  return(.dgraph)
}

#' @export
is.layer <- function(x){ (class(x) == "layer") && (length(class(x)) == 1)}
#' @export
is.node <- function(x){ "node" %in% class(x)}
#' @export
is.dgraph <- function(x){ class(x) == "dgraph"}

# lifts the first layer to dgraph
layer_to_dgraph <- function(.layer){
  .layer <- assign_node_ids(.layer, start = 0)
  attr(.layer, ".id") <- 1
  graph <- layer_to_graph(.layer, graph = NULL)
  .dgraph <- list(.layer)
  attr(.dgraph, ".graph") <- graph
  class(.dgraph) <- "dgraph"
  return(.dgraph)
}

# adds a layer to a dgraph
add_layer <- function(.layer, .dgraph, .id){
  attr(.layer, ".id") <- .id
  g <- attr(.dgraph, ".graph")
  control <- attr(.dgraph, ".dcontrol")
  .layer <- assign_node_ids(.layer, start = max(g$node))
  .dgraph <- c(.dgraph, list(.layer)) # loses attributes??
  graph <- layer_to_graph(.layer, graph = g)
  attr(.dgraph, ".graph") <- graph
  attr(.dgraph, ".dcontrol") <- control
  return(.dgraph)
}

# Add layer to task graph, and update task graph if already exists
layer_to_graph <- function(l, graph = NULL){
  node.ids <- layer_apply(l, select=".id")
  lid <- attr(l, ".id")
  n.nodes <- length(l)
  ntasks <- layer_apply(l, FUN=function(a){prod(sapply(a, length))}, select = ".args")

  if(is.null(graph)){
    dep <- node.ids
    tlow <- c(1, 1+ntasks)[-(n.nodes+1)]
    tup <- cumsum(ntasks)
    graph <- data.frame(layer = rep(attr(l, ".id"), n.nodes), dep=dep, node=node.ids,
                        ntasks = ntasks, tlow=tlow, tup=tup)

  } else {
    sub_graph <- subset(graph, layer == lid - 1)
    start <- max(sub_graph[, "tup"])
    dep <- sub_graph$node
    # create a row for each dependency
    dep.rows <- expand.grid(layer = lid, dep=dep, node=node.ids)
    dep.rows$ntasks <- rep(ntasks, each = length(dep)) *
      rep(sub_graph$ntasks, times = n.nodes)
    dep.rows$tlow <- head(c(start + 1, start + 1 + cumsum(dep.rows$ntasks)), -1)
    dep.rows$tup <- cumsum(dep.rows$ntasks) + start
    graph <- rbind(graph, dep.rows)
  }
  return(graph)
}

#' @export
layer_apply <- function(l, select=".id", FUN=I){
  sapply(l, function(x){FUN(x[[select]])})
}

#' @export
get_node <- function(dgraph, id){
  ids <- sapply(dgraph, function(l){ layer_apply(l, select = ".id") })
  addr <- which(ids == id, arr.ind=T)
  node.id <- addr[1]
  layer.id <- addr[2]
  dgraph[[layer.id]][[node.id]]
}

# @export
assign_node_ids <- function(e, start=0){
  for(i in which(sapply(e, is.node))){
    if(is.null(e[[i]]$.id)) e[[i]]$.id <- start + i
  }
  return(e)
}

#' @export
reps <- function(.dgraph, reps){
  attr(.dgraph, ".dcontrol")["reps"] <- reps
  return(.dgraph)
}







