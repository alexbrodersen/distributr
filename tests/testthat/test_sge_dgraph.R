
fdir <- "tests/tmp_nl"
system("mkdir -p tests/tmp_nl")
system("rm -rf tests/tmp_nl/*")

ff <- function(a, b){a + b}
gg <- function(x, arg1){x^2}
hh <- function(x, arg2){-x}

o <- layer(node(ff, a=1:3, b=1:3), node(ff, a=4:5, b=4:5)) %>%
  layer(node(hh, arg2=1), node(gg, arg1=1)) %>%
  dcontrol() %>% reps(5)

setup.dgraph(o)
system("cat tests/tmp_nl/doone.R")

load("tests/tmp_nl/dgraph.Rdata")
graph <- attr(dgraph, ".graph")
control <- attr(dgraph, ".dcontrol")

setwd("tests/tmp_nl")
for(i in 1:39) {
  cmd <- paste0("Rscript doone.R ", i)
  system(cmd)
}



for(t in 1:13){
  sub_graph <- graph[graph$tlow <= t & graph$tup >= t, ]
  node <- get_node(dgraph, sub_graph$node)
  param.id <- which(sub_graph$tlow:sub_graph$tup == t)
  res.l <- grid_apply(.f = node$.f, node$.args, .paramid = param.id,
                    .reps = control$reps, .mc.cores = control$mc.cores,
                    .verbose = control$verbose)
  fn <- paste0(fdir, "/results/layer", sub_graph$layer, "/node", sub_graph$node, "_t", t, ".Rdata")
  save(res.l, file=fn)
}
