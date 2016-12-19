context("sge dgraph")
if(interactive()) setwd("tests/testthat/")
fdir <- "tmp_nl"


system(paste0("rm -rf ", fdir))

ff <- function(a, b){c(a + b, a - b)}
gg <- function(y, arg1){y^2}
hh <- function(z, arg2){-z}
.reps <- 8

o <- layer(node(ff, a=1:3, b=1:3), node(ff, a=4:5, b=4:5)) %>%
  layer(node(hh, arg2=1), node(gg, arg1=1)) %>%
  control() %>% reps(.reps)

msg <- capture.output(setup(o, dir=fdir))

test_that("sge dgraph setup", {
  expect_true(file.exists("tmp_nl/results/layer1"))
  expect_true(file.exists("tmp_nl/results/layer2"))
  expect_true(file.exists("tmp_nl/SGE_Output"))
  expect_true(file.exists("tmp_nl/submit"))
  expect_true(file.exists("tmp_nl/submit.sh"))
  expect_true(file.exists("tmp_nl/dgraph.Rdata"))
  #load("tmp/arg_grid.Rdata")
  #expect_identical(jobs(out), arg_grid)
  #msg <- capture.output(setup(o, dir=fdir))
})

setwd(fdir)

context("sge_dgraph_doone")

for(i in 1:39) {
  cmd <- paste0("Rscript doone.R ", i, " 1")
  system(cmd, ignore.stdout = T)
}
setwd("../")

test_that("sge dgraph load_results", {
  expect_equal(unlist(load_results("t1.Rdata", fdir)), rep(c(2, 0), .reps))
})

test_that("sge_dgraph_collect", {
  expect_equivalent(collect(o, task = 1, dir = fdir),
                    load_results("t1.Rdata", fdir))

  res1 <- unlist(collect(o, node = 1, dir=fdir))
  ans1 <- unlist(grid_apply(ff, a = 1:3, b = 1:3, .reps = .reps))
  expect_equal(res1, ans1)

  res2 <- unlist(collect(o, node = 2, dir = fdir))
  ans2 <- unlist(grid_apply(ff, a = 4:5, b = 4:5, .reps = .reps))
  expect_equal(res2, ans2)

  res3 <- unlist(collect(o, node = 3, dir = fdir))
  expect_equal(res3, hh(ans1))

  res4 <- unlist(collect(o, node = 4, dir = fdir))
  expect_equal(res4, hh(ans2))

  res5 <- unlist(collect(o, node = 5, dir = fdir))
  expect_equal(res5, gg(ans1))

  res6 <- unlist(collect(o, node = 6, dir = fdir))
  expect_equal(res6, gg(ans2))

  res <- unlist(collect(o, layer = 2, dir = fdir))

  fin.ans <- c(hh(ans1, 1), hh(ans2, 1), gg(ans1, 1), gg(ans2, 1))
  expect_equal(res, fin.ans)

  res2 <- unlist(collect(o, dir = fdir))
  expect_equal(res, res2)
})

context("sge_dgraph_tidy")
test_that("sge dgraph tidy", {
  res.tidy <- collect(o, dir = fdir) %>% tidy(., dir = fdir)
  expect_equal(res.tidy$value, fin.ans)
})

#context("sge_dgraph_collect_node")

# system(paste0("rm -rf ", fdir))
#
# o <- layer(node(ff, a=1:3, b=1:3), node(ff, a=4:5, b=4:5)) %>%
#   layer(node(hh, arg2=1), node(gg, arg1=1)) %>%
#   layer(node(tidy), .reduce = TRUE) %>%
#   control() %>% reps(.reps)
#
# setup(o, dir = fdir)
#
# setwd(fdir)
# ntasks <- max(attr(o, ".graph")$tup)
# for(i in 1:ntasks) {
#   cmd <- paste0("Rscript doone.R ", i)
#   system(cmd)
# }
# setwd("../")
#
# collect(o, dir = fdir, layer = 3)
# load_results("layer3",dir = fdir)

#system(paste0("rm -rf ", fdir))
#setup(o, dir=fdir)
#setwd(fdir)
#for(i in sort(sample(1:39, 15, replace = F))) {
#  cmd <- paste0("Rscript doone.R ", i)
#  system(cmd)
#}
#setwd("../")

#res <- collect(o, dir = fdir)

#context("timing")
#library(microbenchmark)
#times  = microbenchmark::microbenchmark(res.tidy <- collect.dgraph(dir = fdir) %>% tidy.dgraph(dir = fdir))

#library(profvis)
#profvis(
#  res.tidy <- collect.dgraph(dir = fdir) %>% tidy.dgraph(dir = fdir)
#)

