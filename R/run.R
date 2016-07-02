#' @export
run.ggraph <- function(object){
  if(object$backend == "local"){
    run.local(object)
  }
}

#' @export
run.local <- function(object){
  ## find the functions
  fs <- as.numeric(which(sapply(object, isgrid)))
  ## return the levels corresponding to each function
  levels <- as.numeric(sapply(object[fs], function(g){g$.level}))

  # i indexes levels
  # j indexes nodes within current level
  # k indexes nodes within previous level
  # l indexes grid rows within node(s) at previous level
  #   if .level, all k
  #   else k corresponding to .dep
  # gapply.local handles grid rows within current node

  # cnode = current node
  # pnode = previous node
  # res.pnode = results of previous node
  # res.cnode = results of current node
  # res.(pc)level = results of (previous, current) level

  res <- list()

  for(i in 1:max(levels)){
    if(i > 1) res.plevel <- res.clevel

    res.clevel <- list()
    node.ids <- which(levels == i)
    for(j in seq_along(node.ids)){
      res.cnode <- list()
      cnode <- object[[node.ids[j] ]]
      if(i == 1){
        res.cnode[[j]] <- ngapply(node=cnode)
      } else {
        for(k in seq_along(res.plevel)){
          # get results from node k at previous level
          # res.pnode is of length L
          # res.cnode is also of length L
          res.pnode <- res.plevel[[k]]
          res.cnode[[k]] <- lapply(res.pnode, function(a){ ngapply(arg=a, node=cnode)})
        }
      }
      res.clevel[[j]] <- res.cnode
    }
    res[[i]] <- res.clevel
  }
  class(res) <- c("ggraph", "gresults")
  return(res)
}

#' takes a node and runs gapply on f and the grid passing in arg (the results from previous)
#' @export
ngapply <- function(arg=NULL, node){
  if(!is.null(arg)){
    res <- gapply.local(.f = node$.f, node$grid, .args=arg)
  } else {
    res <- gapply.local(.f = node$.f, node$grid)
  }
  return(res)
}
