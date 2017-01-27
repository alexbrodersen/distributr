context("sge grid_apply")
if(interactive()) setwd("tests/testthat/")
fdir <- "tmp"

do.one <- function(a=1, b=2, dat){
  if(a==1) stop("asdf")
  a
}
out <- gapply(do.one, a=1:2, b=2, .reps=2, .verbose=0, .eval = F,
              .args=list(dat=data.frame(rnorm(5), rnorm(5))))

test_that("setup", {
  skip_on_cran()
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
  arg_grid <- readRDS("tmp/arg_grid.Rdata")
  expect_identical(jobs(out), arg_grid)
})



test_that("clean", {
  msg <- capture.output(clean("tmp"))
  expect_equal(length(dir("tmp")), 0)
})



test_that("setup verbose", {
  skip_on_cran()
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
  skip_on_cran()
  msg <- capture.output(clean("tmp"))
  msg <- capture.output(out <- setup(out, .dir="tmp", .reps = 5))

  ## This is a hack to get the tests to run from this directory
  setwd("tmp")
  system("Rscript doone.R 1 1", ignore.stdout = T)
  system("Rscript doone.R 2 1", ignore.stdout = T)
  setwd("../")

  outc <- collect(out, dir = "tmp") %>% tidy
  out <- gapply(do.one, a=1:2, b=2, .reps=5, .verbose=0, .eval = T)
  expect_equivalent(select(outc,-.sge_id), out)

  outf <- collect(out, dir="tmp", filter="a < 5") %>% tidy
  ans <- outc %>% filter(a < 5)
  expect_equal(outf, ans)

  outf <- collect(out, dir="tmp", filter= ~a < 5) %>% tidy
  ans <- outc %>% filter(a < 5)
  expect_equal(outf, ans)

  set.seed(104)
  outs <- collect(out, dir = "tmp", sample=1) %>% tidy
  expect_equal(nrow(outs), nrow(outc)/2)
  expect_true(all(outs$.sge_id) == 1)

  outr <- collect(out, dir = "tmp", regex="1") %>% tidy
  expect_equal(outr, outs)
  outr <- collect(out, dir = "tmp", regex="2") %>% tidy
  expect_true(all(outr$.sge_id == 2))
  outr <- collect(out, dir = "tmp", regex="[1-2]") %>% tidy
  expect_equal(outr, outc)

  outfr <- collect(out, dir = "tmp", filter="a < 5", regex="2") %>% tidy
  ans <- outc %>% filter(a == 2)
  expect_equivalent(outfr, ans)

  set.seed(104)
  outfs <- collect(out, dir = "tmp", filter="b == 2", sample = 1) %>% tidy
  expect_equal(outfs, outs)
})

test_that("jobs can have different numbers of completed replications", {
  skip_on_cran()
  res.l <- readRDS("tmp/results/2.Rdata")
  res.l <- res.l[c(1, 3)]
  saveRDS(res.l, file="tmp/results/2.Rdata")
  rm(res.l)

  outc <- collect(out, dir = "tmp") %>% tidy
  expect_equal(nrow(outc), 7)
  ans <- c(rep(NA, 5), 2, 2)
  expect_equal(outc$value, ans)
})


test_that("add_jobs", {
  skip_on_cran()
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
  skip_on_cran()
  msg <- capture.output(ff <- filter_jobs(out, .dir="tmp", a==1))
  sub <- readLines("tmp/submit")
  expect_equal(sub[grep("-t", sub)], "#$ -t 1:1")

  out2 <- add_jobs(out, a=5, b=c(2, 4))
  msg <- capture.output(filter_jobs(out2, .dir="tmp", a < 2 | b > 2))
  sub <- readLines("tmp/submit")
  expect_equal(sub[grep("-t", sub)], "#$ -t 1, 4")
})

test_that("overwriting prompts a msg", {
  skip_on_cran()
  if(interactive()){
    out <- gapply(do.one,a=1:2,b=2, .reps=2, .verbose=0, .eval = F)
    out3 <- setup(out, .dir="tmp", .reps = 6)
    out <- gapply(do.one, a=1:4, b=2, .reps=5, .verbose=0, .eval = F)
    out3 <- setup(out, .dir="tmp", .reps = 6)
  }
})

test_that("test_job", {
  skip_on_cran()
   msg <- capture.output(clean("tmp/"))
   msg <- capture.output(plan <- setup(out, .dir="tmp", .reps = 5, .verbose=0))
   setwd("tmp/")
   msg <- capture.output(test_job(2))
   setwd("../")
   res <- collect(plan, dir="tmp") %>% tidy
   res$.sge_id <- NULL
   msg <- capture.output(ans <- gapply(do.one, a=2, b=2, .reps=5, .verbose=0, .eval = T,
                 .args=list(dat=data.frame(rnorm(5), rnorm(5)))))
   expect_true(all(res==ans))
})

test_that("setup seeds", {
  do.one <- function(a=1, b=2, dat){
    if(a==1) stop("asdf")
    rnorm(1, a, .01)
  }
  out <- gapply(do.one, a=1:2, b=2, .reps=2, .verbose=0, .eval = F,
                .args=list(dat=data.frame(rnorm(5), rnorm(5))))

  skip_on_cran()
  system(paste0("mkdir -p ", fdir))
  system(paste0("rm -rf ", fdir, "/*"))

  msg <- capture.output(out <- setup(out, .dir="tmp", .reps = 6, .seed=104))
  expect_true(file.exists("tmp/seeds.Rdata"))

  setwd("tmp")
  msg <- system("Rscript doone.R 1 1", ignore.stdout = T)
  msg <- system("Rscript doone.R 2 1", ignore.stdout = T)
  setwd("../")

  o1 <- collect(out, dir = "tmp")
  system(paste0("rm -rf ", fdir, "/*"))
  msg <- capture.output(out <- setup(out, .dir="tmp", .reps = 6, .seed=104))

  ## This is a hack to get the tests to run from this directory
  setwd("tmp")
  msg <- system("Rscript doone.R 1 1", ignore.stdout = T)
  msg <- system("Rscript doone.R 2 1", ignore.stdout = T)
  setwd("../")
  o2 <- collect(out, dir = "tmp")
  expect_equal(o1, o2)

})

if(interactive()) setwd("../../")


