context("sge grid_apply")
if(interactive()) setwd("tests/testthat/")
fdir <- "tmp"


do.one <- function(a=1,b=2){
  if(a==1) stop("asdf")
  a
}
out <- gapply(do.one,a=1:2,b=2, .reps=2, .verbose=0, .eval = F)

system(paste0("mkdir -p ", fdir))
system(paste0("rm -rf ", fdir, "/*"))

setup(out, .dir="tmp", .reps = 6)

## This is a hack to get the tests to run from this directory
setwd("tmp")
system("Rscript doone.R 1 1")
system("Rscript doone.R 2 1")
setwd("../")

clean("tmp")

out <- setup(out, .dir="tmp", .reps = 5, .verbose=2)
setwd("tmp")
system("Rscript doone.R 1 1")
system("Rscript doone.R 2 1")
setwd("../")

clean("tmp")

out <- setup(out, .dir="tmp", .reps = 5, .verbose=3)
setwd("tmp")
system("Rscript doone.R 1 1")
system("Rscript doone.R 2 1")
setwd("../")

context("test_collect_sge")

outc <- collect(out, dir = "tmp") %>% tidy
out <- gapply(do.one,a=1:2,b=2, .reps=5, .verbose=0, .eval = T)
expect_equivalent(select(outc, -chunk), out)

if(interactive()) setwd("../../")

context("test add_jobs")

out2 <- add_jobs(out, a=5)
new_grid <- attr(out2, "arg_grid")
expect_equal(new_grid, dplyr::bind_rows(attr(out, "arg_grid"), expand.grid(a=5)))
out2 <- add_jobs(out, a=5, b=c(2, 4))
new_grid <- attr(out2, "arg_grid")
expect_equal(new_grid,
    dplyr::bind_rows(attr(out, "arg_grid"), expand.grid(a=5, b=c(2, 4))))

setup(out2, .dir="tmp", .reps = 5, .verbose=3)
setwd("tmp")
system("Rscript doone.R 1 1")
system("Rscript doone.R 2 1")
system("Rscript doone.R 3 1")
system("Rscript doone.R 4 1")
setwd("../")

outc <- collect(out, dir = "tmp") %>% tidy
out <- gapply(do.one,a=1:2,b=2, .reps=5, .verbose=0, .eval = T)
out2 <- gapply(do.one,a=5,b=c(2,4), .reps=5, .verbose=0, .eval = T)

expect_equivalent(rbind(out, out2), select(outc, -chunk))

context("test submit_jobs")

filter_jobs(out, .dir="tmp", a==1)
sub <- readLines("tmp/submit")
expect_equal(sub[grep("-t", sub)], "#$ -t 1:1")

out2 <- add_jobs(out, a=5, b=c(2, 4))
filter_jobs(out2, .dir="tmp", a < 2 | b > 2)
sub <- readLines("tmp/submit")

