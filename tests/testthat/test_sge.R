context("sge grid_apply")
if(interactive()) setwd("tests/testthat/")
fdir <- "tmp"

do.one <- function(a=1,b=2){
  if(a==1) stop("asdf")
  a
}
out <- gapply(do.one, a=1:2, b=2, .reps=2, .verbose=0, .eval = F)

test_that("setup", {
  system(paste0("mkdir -p ", fdir))
  system(paste0("rm -rf ", fdir, "/*"))

  msg <- capture.output(out <- setup(out, .dir="tmp", .reps = 6))

  ## This is a hack to get the tests to run from this directory
  setwd("tmp")
  msg <- system("Rscript doone.R 1 1", ignore.stdout = T)
  msg <- system("Rscript doone.R 2 1", ignore.stdout = T)
  setwd("../")

  expect_true(file.exists("tmp/results/1.Rdata"))
  expect_true(file.exists("tmp/results/2.Rdata"))
  expect_true(file.exists("tmp/SGE_Output"))
  expect_true(file.exists("tmp/submit"))
  expect_true(file.exists("tmp/arg_grid.Rdata"))
  load("tmp/arg_grid.Rdata")
  expect_identical(jobs(out), arg_grid)
})

test_that("inconsistent .reps produces error", {
  system(paste0("mkdir -p ", fdir))
  system(paste0("rm -rf ", fdir, "/*"))

  msg <- capture.output(out <- setup(out, .dir="tmp", .reps = 6))

})

test_that("clean", {
  msg <- capture.output(clean("tmp"))
  expect_equal(length(dir("tmp")), 0)
})



test_that("setup verbose", {
  # This just tests that the argumnt .verbose works; can't test it locally
  # outside of SGE.

  msg <- capture.output(out <- setup(out, .dir="tmp", .reps = 5, .verbose=2))
  setwd("tmp")
  system("Rscript doone.R 1 1", ignore.stdout = T)
  system("Rscript doone.R 2 1", ignore.stdout = T)
  setwd("../")

  msg <- capture.output(clean("tmp"))

  msg <- capture.output(out <- setup(out, .dir="tmp", .reps = 5, .verbose=3))
  setwd("tmp")
  system("Rscript doone.R 1 1", ignore.stdout = T)
  system("Rscript doone.R 2 1", ignore.stdout = T)
  setwd("../")
})

test_that("collect_sge ", {
  msg <- capture.output(out <- setup(out, .dir="tmp", .reps = 5))

  ## This is a hack to get the tests to run from this directory
  setwd("tmp")
  system("Rscript doone.R 1 1", ignore.stdout = T)
  system("Rscript doone.R 2 1", ignore.stdout = T)
  setwd("../")

  outc <- collect(out, dir = "tmp") %>% tidy
  out <- gapply(do.one, a=1:2, b=2, .reps=5, .verbose=0, .eval = T)
  expect_equivalent(select(outc,-.sge_id), out)
})


test_that("add_jobs", {
  out2 <- add_jobs(out, a=5)
  new_grid <- attr(out2, "arg_grid")
  ans <- dplyr::bind_rows(attr(out, "arg_grid"), expand.grid(a=5))
  ans$.sge_id=1:nrow(ans)
  expect_equal(new_grid, ans)
  out2 <- add_jobs(out, a=5, b=c(2, 4))
  new_grid <- attr(out2, "arg_grid")
  ans <- dplyr::bind_rows(attr(out, "arg_grid"), expand.grid(a=5, b=c(2, 4)))
  ans$.sge_id <- 1:nrow(ans)
  expect_equal(new_grid, ans)

  msg <- capture.output(clean(dir="tmp"))
  msg <- capture.output(out2 <- setup(out2, .dir="tmp", .reps = 5, .verbose=3))

    setwd("tmp")
  for(i in 1:4) system(paste0("Rscript doone.R ", i," 1"), ignore.stdout = T)
  setwd("../")

  outc <- collect(out2, dir = "tmp") %>% tidy
  out <- gapply(do.one,a=1:2,b=2, .reps=5, .verbose=0, .eval = T)
  out2 <- gapply(do.one,a=5,b=c(2,4), .reps=5, .verbose=0, .eval = T)

  expect_equivalent(rbind(out, out2), select(outc, -.sge_id))
})

test_that("filter jobs", {
  msg <- capture.output(ff <- filter_jobs(out, .dir="tmp", a==1))
  sub <- readLines("tmp/submit")
  expect_equal(sub[grep("-t", sub)], "#$ -t 1:1")

  out2 <- add_jobs(out, a=5, b=c(2, 4))
  msg <- capture.output(filter_jobs(out2, .dir="tmp", a < 2 | b > 2))
  sub <- readLines("tmp/submit")
  expect_equal(sub[grep("-t", sub)], "#$ -t 1, 4")
})

test_that("overwriting prompts a msg", {
  if(interactive()){
    out <- gapply(do.one,a=1:2,b=2, .reps=2, .verbose=0, .eval = F)
    out3 <- setup(out, .dir="tmp", .reps = 6)
    out <- gapply(do.one, a=1:4, b=2, .reps=5, .verbose=0, .eval = F)
    out3 <- setup(out, .dir="tmp", .reps = 6)
  }
})




if(interactive()) setwd("../../")


