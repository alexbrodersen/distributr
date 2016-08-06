
#' @export
"+.layer" <- function(e1, e2){
  add_layer(e1, e2)
}

#' @export
layer <- function(..., .id=NULL){
  # check that all ... are layers or options
  layer <- list(...)
  stopifnot(sapply(layer, is.node) | sapply(layer, is.doption))
  attr(layer, ".id") <- .id
  class(layer) <- "layer"
  return(layer)
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
is.dcontrol <- function(x){ any(grepl("dcontrol", class(x)))}
#' @export
is.layer <- function(x){ (class(x) == "layer") && (length(class(x)) == 1)}
#' @export
is.node <- function(x){ "node" %in% class(x)}
#' @export
is.dgraph <- function(x){ class(x) == "dgraph"}

#' @export
add_layer <- function(e1, e2){
  if(is.dgraph(e1) && is.layer(e2)){

    if(is.null(e2$.id)) attr(e2, ".id") <- max(sapply(e1, function(l){attr(l, ".id")})) + 1
    g <- attr(e1, ".graph")
    e2 <- assign_node_ids(e2, start = max(g$node))
    o <- c(e1, list(e2))
    graph <- layer_to_graph(e2, graph = g)
    attr(o, ".graph") <- graph
    class(o) <- "dgraph"

  } else if(is.layer(e1) && is.layer(e2)){

    # Assign layer ids if not present
    if(is.null(e1$.id)) attr(e1, ".id") <- 1
    if(is.null(e2$.id)) attr(e2, ".id") <- 2

    # Assign node ids if not present
    e1 <- assign_node_ids(e1, start = 0)
    e2 <- assign_node_ids(e2, start = max(layer_apply(e1, select=".id")))

    # Generate task_map from layer; since this is layer 1 a graph has to be made from e1 first
    graph <- layer_to_graph(e2, graph = layer_to_graph(e1))
    o <- list(e1, e2)
    attr(o, "graph") <- graph
    class(o) <- "dgraph"
  } else if(is.dcontrol(e2)){
    # need to create a node/graph first before options, I think that's reasonable.
    attr(e1, "dcontrol") <- e2
  } else {
    print("cant add")
  }
  return(o)
}

# Add layer to graph, and update graph if already exists
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

layer_apply <- function(l, select=".id", FUN=I){
  sapply(l, function(x){FUN(x[[select]])})
}

assign_node_ids <- function(e, start=0){
  for(i in which(sapply(e, is.node))){
    if(is.null(e[[i]]$.id)) e[[i]]$.id <- start + i
  }
  return(e)
}

reps <- function(reps){
  class(reps) <- c("dcontrol", "layer")
  return(reps)
}

#' @export
dcontrol <- function(tidy=NULL, backend="local", cache="all"){
  o <- list(tidy=tidy, backend=backend, cache=cache)
  class(o) <- c("dcontrol", "layer")
  return(o)
}





