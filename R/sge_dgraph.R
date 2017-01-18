#' @export
setup.dgraph <- function(object, dir=getwd(), .mc.cores=1, .verbose=1,
                         .queue="long",
                         .script.name="doone.R",
                         .job.name="patr1ckm",
                         .out.dir="SGE_Output",
                         .email.options="a",
                         .email.addr="patr1ckm.crc.nd.edu",
                         .shell="bash", ...){
  # check whether has '/' on the end?
  fdir <- paste0(dir, "/")
  cmd <- paste0("mkdir -p ",  fdir)
  mysys(cmd)
  # write the graph to a file
  dgraph <- object
  saveRDS(dgraph, file = paste0(fdir, "dgraph.Rdata"))
  graph <- attr(dgraph, ".graph")

  # mkdir(s) for caching results
  cmd <- paste0("mkdir -p ", fdir, "results")
  mysys(cmd)
  cmd <- paste0("mkdir -p ", fdir, "results/layer", unique(graph$layer))
  for(i in 1:length(cmd)){ mysys(cmd[i])}

  # mkdir for SGE output
  cmd <- paste0("mkdir -p ", fdir, "SGE_Output")
  mysys(cmd)

  # write the submission scripts

  write_submit_dgraph(fdir, dgraph, script.name=.script.name,
                      mc.cores=.mc.cores,
               out.dir = .out.dir, email = .email.options,
               email.addr = .email.addr, shell = .shell)

  write_doone_dgraph(dgraph, dir=fdir, script.name = .script.name)


}

write_submit_dgraph <- function(dir, dgraph, script.name="doone.R",
                                mc.cores=1, queue="long",
                                out.dir="SGE_Output", email="a",
                                email.addr="patr1ckm.crc.nd.edu", shell="bash"){

  graph <- attr(dgraph, ".graph")
  submit_dir <- paste0(dir, "submit")
  submit_all_fn <- paste0(dir, "submit.sh")
  mysys(paste0("mkdir -p ", submit_dir))
  mysys(paste0("touch ", submit_all_fn))
  cat(paste0("#!/bin/", shell), file=submit_all_fn, fill=T)

  for(i in 1:nrow(graph)){
    ### Create a submission script for each node
    node <- graph[i, ]
    job_name <- paste0("node_", i)
    submit_name <- paste0("submit_node_", i)

    submit_node <- paste0(
      "#!/bin/", shell, " \n",
      "#$ -M ", email.addr, "\n",
      "#$ -m ", email, "\n",
      "#$ -pe smp ",min(mc.cores), "-", max(mc.cores), "\n",
      "#$ -q ", queue, "\n",
      "#$ -N ", job_name, "\n",
      "#$ -t ", node$tlow, ":", node$tup, "\n",
      "#$ -o ", out.dir, " \n\n",
      "Rscript ", script.name, " $SGE_TASK_ID $NSLOTS")
    cat(submit_node, file=paste0(submit_dir, "/", submit_name))

    ### Update submit.sh which executes all scripts

    # Hold for dependencies, otherwise don't
    hold <- NULL
    if(node$dep != node$node.id){
      hold <- paste0("-hold_jid node_", node$dep)
    }
    qsub_cmd <- paste0("qsub ", hold, " submit/", submit_name)

    cat(qsub_cmd, file = submit_all_fn, append = T, fill = T)
  }

  # Make executable
  mysys(paste0("chmod +x ", submit_all_fn))
}

write_doone_dgraph <- function(dgraph, dir, script.name="doone.R"){
  doone <- paste0("
  suppressMessages(library(distributr))
  args <- as.numeric(commandArgs(trailingOnly=TRUE))
  t <- args[1]
  ncores <- args[2]
  dgraph <- readRDS(\"dgraph.Rdata\")
  graph <- attr(dgraph, \".graph\")
  sub_graph <- graph[graph$tlow <= t & graph$tup >= t, ]
  if(sub_graph$dep == sub_graph$node.id){
    # load nothing
    node <- get_node(dgraph, sub_graph$node.id)
    control <- attr(dgraph, \".control\")
    param.id <- which(sub_graph$tlow:sub_graph$tup == t)
    res.l <- grid_apply(.f = node$.f, node$.args, .paramid = param.id,
                        .reps = control$.reps, .mc.cores = control$.mc.cores,
                        .verbose = control$.verbose)

  } else {
    # load previous results
    if(sub_graph$dep > 0) {
      dep_graph <- graph[graph$node.id == sub_graph$dep, ]
    } else {
      dep_graph <- sub_graph
    }

    node <- get_node(dgraph, sub_graph$node.id)
    control <- attr(dgraph, \".control\")

    # which row of parameters to run within node
    param.id <- which(sub_graph$tlow:sub_graph$tup == t)

    # which task to load
    prev_t <- dep_graph$tlow:dep_graph$tup
    all <- append(list(t = prev_t), node$.args) %>% expand.grid
    args <- all[param.id, ]


    if(dep_graph$dep == 0){
      layers <- list.files(\"", dir, "results\")
      # the first layer will never have dep == 0
      prev_layer <- dep_graph$layer - 1
      prev_res <- collect(dgraph, layer = prev_layer)

      args <- purrr::flatten(list(prev_res, args[-1]))
      names(args)[1] <- names(formals(node$.f))[1]

      # just call f on the previous results and args
      res.l <- do.call(node$.f, args)

    } else {
      prev_res <- paste0(\"t\", args$t, \".Rdata\") %>%
        load_results(.)

      args <- purrr::flatten(list(prev_res, args[-1]))
      names(args)[1] <- names(formals(node$.f))[1]

      res.l <- grid_apply(.f = node$.f, args,
        .reps = 1, .mc.cores = ncores, .verbose = control$.verbose)
    }
  }
  fn <- paste0(\"results/layer\", sub_graph$layer, \"/node_pos\", sub_graph$node.pos, \"_t\", t, \".Rdata\")
  saveRDS(res.l, file=fn)
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
    res.l <- readRDS(fn)
    res.node[[i]] <- res.l
  }
  return(res.node)
}

#' Collect layers, nodes, or tasks from a completed dgraph
#' @param x \code{dgraph object}
#' @param dir director
#' @param layer which layer to collect
#' @param node which node to collect
#' @param task which task id to collect
#' @export
#' @importFrom gtools mixedsort
collect.dgraph <- function(x, dir = getwd(), layer=NULL, node=NULL, task=NULL, ...){
  # Load a particular layer, node or task
  if(any(!is.null(layer) | !is.null(node) | !is.null(task))){
    if(!is.null(task)) {
      reg <- paste0("_t", task, ".Rdata")
    } else if(!is.null(node)){
      reg <- paste0("node_pos", node)
    } else if(!is.null(layer)){
      reg <- paste0("layer", layer)
    }
    res <- load_results(reg, dir=dir)

  } else {
    # all are NULL, default to loading last layer
    layers <- list.files(paste0(dir, "/results"))
    last.layer <- tail(gtools::mixedsort(layers), n=1)
    res <- load_results(last.layer, dir = dir)
  }

  class(res) <- c(class(res), "dgraph")

  arg_grid <- expand_grid_dgraph(x, layer.id = layer)

  ## here: select completed conditions and update arg_grid

  ## since each file has t in the index, and t indexes rows of arg_grid, grab t and subset
  # lapply(dir, )

  .reps <- attr(x, ".control")$.reps
  attr(res, "arg_grid") <- arg_grid
  attr(res, ".reps") <- .reps
  return(res)
}





