context("sge dgraph")
# if interactive: setwd("tests/testthat/")
fdir <- "tmp_nl"


system(paste0("rm -rf ", fdir))

ff <- function(a, b){c(a + b, a - b)}
gg <- function(y, arg1){y^2}
hh <- function(z, arg2){-z}
.reps <- 8

o <- layer(node(ff, a=1:3, b=1:3), node(ff, a=4:5, b=4:5)) %>%
  layer(node(hh, arg2=1), node(gg, arg1=1)) %>%
  control() %>% reps(.reps)

context("setup.dgraph")
setup.dgraph(o, dir=fdir)

setwd(fdir)

context("doone.R")

for(i in 1:39) {
  cmd <- paste0("Rscript doone.R ", i)
  system(cmd)
}
setwd("../")

context("load_results")
expect_equal(unlist(load_results("t1.Rdata", fdir)), rep(c(2, 0), .reps))

context("collect.dgraph")
expect_equal(collect.dgraph(task = 1, dir = fdir), load_results("t1.Rdata", fdir))

res1 <- unlist(collect.dgraph(node = 1, dir=fdir))
ans1 <- unlist(grid_apply(ff, a = 1:3, b = 1:3, .reps = .reps))
expect_equal(res1, ans1)

res2 <- unlist(collect.dgraph(node = 2, dir = fdir))
ans2 <- unlist(grid_apply(ff, a = 4:5, b = 4:5, .reps = .reps))
expect_equal(res2, ans2)

res3 <- unlist(collect.dgraph(node = 3, dir = fdir))
expect_equal(res3, hh(ans1))

res4 <- unlist(collect.dgraph(node = 4, dir = fdir))
expect_equal(res4, hh(ans2))

res5 <- unlist(collect.dgraph(node = 5, dir = fdir))
expect_equal(res5, gg(ans1))

res6 <- unlist(collect.dgraph(node = 6, dir = fdir))
expect_equal(res6, gg(ans2))

res <- unlist(collect.dgraph(layer = 2, dir = fdir))

fin.ans <- c(hh(ans1, 1), hh(ans2, 1), gg(ans1, 1), gg(ans2, 1))
expect_equal(res, fin.ans)

res2 <- unlist(collect.dgraph(dir = fdir))
expect_equal(res, res2)

context("tidy.dgraph")
res.tidy <- collect.dgraph(dir = fdir) %>% tidy.dgraph(dir = fdir)
expect_equal(res.tidy$value, fin.ans)


context("timing")
#library(microbenchmark)
#times  = microbenchmark::microbenchmark(res.tidy <- collect.dgraph(dir = fdir) %>% tidy.dgraph(dir = fdir))

#library(profvis)
#profvis(
#  res.tidy <- collect.dgraph(dir = fdir) %>% tidy.dgraph(dir = fdir)
#)

