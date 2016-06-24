context("test_collect")

do.one <- function(a=1,b=2){
  if(a==1) stop("asdf")
  a
}
out <- gapply(do.one,a=1:2,b=2, .reps=2, .verbose=0)

system("mkdir -p tests/tmp")
system("rm -rf tests/tmp/*")

setup(out, dir="tests/tmp", .reps = 2, .chunks = 1, .verbose=2)
setwd("tests/tmp")
system("Rscript doone.R 1 ")
system("Rscript doone.R 2 ")
setwd("../../")

#system("ls tests/tmp/results")
#system("cat tests/tmp/doone.R")

#system("rm -rf tests/tmp/*")

outc <- collect("tests/tmp")
summary(out)

expect_true(identical(out, outc))


