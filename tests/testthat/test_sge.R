context("sge grid_apply")
# if interactive: setwd("tests/testthat/")
fdir <- "tmp"


do.one <- function(a=1,b=2){
  if(a==1) stop("asdf")
  a
}
out <- gapply(do.one,a=1:2,b=2, .reps=2, .verbose=0, .eval = F)

system(paste0("mkdir -p ", fdir))
system(paste0("rm -rf ", fdir, "/*"))

setup(out, dir="tmp", .reps = 6)

## This is a hack to get the tests to run from this directory
setwd("tmp")
system("Rscript doone.R 1 ")
system("Rscript doone.R 2 ")
setwd("../")

clean("tmp")

setup(out, dir="tmp", .reps = 5, .verbose=2)
setwd("tmp")
system("Rscript doone.R 1 ")
system("Rscript doone.R 2 ")
setwd("../")

clean("tmp")

setup(out, dir="tmp", .reps = 5, .verbose=3)
setwd("tmp")
system("Rscript doone.R 1 ")
system("Rscript doone.R 2 ")
setwd("../")

context("test_collect_sge")

outc <- collect("tmp")
out <- gapply(do.one,a=1:2,b=2, .reps=5, .verbose=0, .eval = T)
expect_equivalent(outc, out)

# if interactive: setwd("../../")


