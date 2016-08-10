
system("mkdir -p tests/tmp_nl")
system("rm -rf tests/tmp_nl/*")
setup.dgraph(o, dir="tests/tmp_nl")

ff <- function(a, b){a + b}
gg <- function(x, arg1){x^2}
hh <- function(x, arg2){-x}

o <- layer(node(ff, a=1:3, b=1:3), node(ff, a=4:5, b=4:5)) %>%
  layer(node(hh, arg2=1), node(gg, arg1=1)) %>%
  dcontrol() %>% reps(5)

load("dgraph.Rdata")
graph <- attr(dgraph, ".graph")
control <- attr(dgrpah, ".dcontrol")

for(t in 1:20){
  sub_graph <- graph[graph$tlow <= t & graph$tup >= t, ]
  node <- get_node(dgraph, sub_graph$node)
  res.l <- grid_apply(.f = node$.f, node$.args, .paramid = t,
                    .reps = control$reps, .mc.cores = control$mc.cores,
                    .verbose = control$verbose)
  fn <- paste0("results/layer", sub_graph$layer, "/node", sub_graph$node, "_t", t, ".Rdata")
}
