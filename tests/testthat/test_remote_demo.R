

do.one <- function(a=1,b=2){
  if(a==1) stop("asdf")
  a
}
out <- grid_apply(do.one,a=1:2,b=2, .reps=2, .verbose=0, .eval = F)

#setup(out, dir = "ga_test", .reps = 5, .mc.cores = 3)


