

do.one <- function(a=1,b=2){
  if(a==1) stop("asdf")
  a
}
out <- grid_apply(do.one,a=1:2,b=2, .reps=2, .verbose=0, .eval = F)

#setup(out, dir = "ga_test", .reps = 5, .mc.cores = 3)

ff <- function(a, b){a + b}
gg <- function(x, arg1){x^2}
hh <- function(x, arg2){-x}

o <- layer(node(ff, a=1:3, b=1:3), node(ff, a=4:5, b=4:5)) %>%
  layer(node(hh, arg2=1), node(gg, arg1=1)) %>%
  dcontrol() %>% reps(5)

#setup.dgraph(o, dir = "nl_test", .mc.cores = 2)
