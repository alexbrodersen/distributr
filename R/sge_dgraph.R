#' @export
setup.dgraph <- function(dgraph, dir=getwd(), .mc.cores=1, .verbose=1,
                         .queue="long",
                         .script.name="doone.R",
                         .job.name="patr1ckm",
                         .out.dir="SGE_Output",
                         .email.options="a",
                         .email.addr="patr1ckm.crc.nd.edu",
                         .shell="bash"){
  dir <- paste0(dir, "/")
  # write the graph to a file
  save(dgraph, file = paste0(dir, "dgraph.Rdata"))
  graph <- attr(dgraph, ".graph")

  # mkdir(s) for caching results
  cmd <- paste0("mkdir -p ", dir, "results")
  mysys(cmd)
  cmd <- paste0("mkdir -p ", dir, "results/layer", unique(graph$layer))
  for(i in 1:length(cmd)){ mysys(cmd[i])}

  # mkdir for SGE output
  cmd <- paste0("mkdir -p ", dir, "SGE_Output")
  mysys(cmd)

  # write the submit script

  write.submit(dir, script.name=.script.name, mc.cores=.mc.cores, tasks=max(graph$tup),
               job.name=.job.name,
               out.dir = .out.dir,
               email = .email.options,
               email.addr = .email.addr,
               shell = .shell)

  write.do.one.dgraph(dgraph, dir=dir, script.name = .script.name)
}

write.do.one.dgraph <- function(dgraph, dir, script.name="doone.R"){
  doone <- paste0("
  library(distributr)
  args <- as.numeric(commandArgs(trailingOnly=TRUE))
  t <- args[1]
  load(\"dgraph.Rdata\")
  graph <- attr(dgraph, \".graph\")
  sub_graph <- graph[graph$tlow <= t & graph$tup >= t, ]
  if(sub_graph$dep == sub_graph$node){
    # load nothing
    node <- get_node(dgraph, sub_graph$node)
    control <- attr(dgraph, \".dcontrol\")
    param.id <- which(sub_graph$tlow:sub_graph$tup == t)
    res.l <- grid_apply(.f = node$.f, node$.args, .paramid = param.id,
                        .reps = control$reps, .mc.cores = control$mc.cores,
                        .verbose = control$verbose)

  } else {
    # load previous results
    dep_graph <- graph[graph$node == sub_graph$dep, ]
    node <- get_node(dgraph, sub_graph$node)
    control <- attr(dgraph, \".dcontrol\")

    # which row of parameters to run within node
    param.id <- which(sub_graph$tlow:sub_graph$tup == t)

    # which task to load
    prev_t = dep_graph$tlow:dep_graph$tup
    all <- append(list(t = prev_t), node$.args) %>% expand_grid %>% purrr::transpose(.)
    args <- all[[param.id]]

    prev_res <- paste0(\"t\", args$t, \".Rdata\") %>%
      load_results(.)

    res.l <- grid_apply(.f = node$.f, append(x=prev_res, args[-1]),
               .reps = 1, .mc.cores = control$mc.cores, .verbose = control$verbose)
  }
  fn <- paste0(\"results/layer\", sub_graph$layer, \"/node\", sub_graph$node, \"_t\", t, \".Rdata\")
  save(res.l, file=fn)
  ")
  cat("cat ", paste0(dir, "doone.R"), fill=T)
  cat(doone, file=paste0(dir, "/", script.name))
}

#' @export
load_results <- function(regex, dir=getwd()){
  fns <- list.files(paste0(dir, "/results/"), recursive = T)
  fl <- fns[grep(regex, fns)]
  res.node <- list()
  for(i in 1:length(fl)){
    fn <- paste0(dir, "/results/", fl[i])
    load(fn)
    res.node[[i]] <- res.l
  }
  return(res.node)
}

collect.dgraph <- function(layer=NULL, node=NULL, task=NULL, dir = getwd()){
  ldir <- paste0(dir, "/results/layer", layer, "/")
  load(paste0(dir, "/dgraph.Rdata"))
  tasks <- gtools::mixedsort(paste0(ldir,list.files(ldir)))


  cond.l <- list()           # list of the results from each condition

  for(i in 1:length(conds.files)){
    fn <- paste0(conds.files[i])
    load(fn)
    cond.l[[i]] <- res.l
  }

  cond.l <- unlist(cond.l, recursive=F)
  cond.idx <- as.numeric(gsub(".Rdata", "", basename(conds.files))) # completed conditions
  cond.grid <- param.grid[cond.idx,]

  err <- lapply(cond.l, function(r){attr(r, "err")})
  err.id <-  which(unlist(lapply(err, function(x){!is.null(x)})))
  err.list <- err[err.id]
  names(err.list) <- err.id

  warn <- lapply(cond.l, function(r){attr(r, "warn")})
  warn.id <- which(unlist(lapply(warn, function(x){!is.null(x)})))
  warn.list <- warn[warn.id]
  names(warn.list) <- warn.id

  value <- as.data.frame(do.call(rbind, cond.l))

}
