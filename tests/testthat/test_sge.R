do.one <- function(a=1,b=2){
  if(a==1) stop("asdf")
  a
}
out <- gapply(do.one,a=1:2,b=2, .reps=2, .verbose=0)

system("mkdir -p tests/tmp")
system("rm -rf tests/tmp/*")

setup(out, dir="tests/tmp/", .reps = 6, .chunks = 3)

## This is a hack to get the tests to run from this directory
setwd("tests/tmp")
system("Rscript doone.R 1 ")
system("Rscript doone.R 2 ")
setwd("../../")

clean("tests/tmp/")

setup(out, dir="tests/tmp/", .reps = 5, .chunks = 3, .verbose=2)
setwd("tests/tmp")
system("Rscript doone.R 1 ")
system("Rscript doone.R 2 ")
setwd("../../")

clean("tests/tmp/")

setup(out, dir="tests/tmp/", .reps = 5, .verbose=3)
setwd("tests/tmp")
system("Rscript doone.R 1 ")
system("Rscript doone.R 2 ")
setwd("../../")

clean("tests/tmp/")
## Let's try a devious one with errors
#do.one <- function(a=1,b=2,...){
#  stopifnot(runif(1) < .5)
#  return(c(a+b,a-b))
#}
#out <- gapply(do.one,reps=2, a=1:2,b=2,verbose=0)


