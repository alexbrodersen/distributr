context("sge monitor")

xml <- readRDS("data/xml_samples.rds")
use <- readRDS("data/sge_usage_strings.rds")
if(interactive()) setwd("tests/testthat/")

test_that("parse_qstat works with tasks", {
  runs <- lapply(xml$info, parse_qstat)
})

test_that("parse_usage works with tasks", {
  df <- parse_usage(use$use1)
  expect_equal(nrow(df), 72)
  expect_true(all(df$status == "r"))
  expect_true(all(df$job_id == 744755))
})

test_that("parse_usage works with N/A maxvmem", {
  x <- parse_usage(use$use2)
  expect_equal(nrow(x), 24)
  expect_true(all(is.na(x$maxvmem)))
  expect_true(all(is.na(x$vmem)))
  expect_true(all(x$mem == 0))
  expect_true(all(x$wallclock == 0))
  expect_true(all(x$cpu == 0))
})

test_that("parse_usage filters exited jobs", {
  x <- parse_usage(use$use3)
  expect_true(nrow(x) == 19)
})

test_that("parse_usage returns NULL with no running jobs", {
  x <- parse_usage(use$use4)
  expect_equal(x, data.frame())
})

test_that("parse_usage works with no tasks", {
  x <- parse_usage(use$use5)
  expect_true(nrow(x) == 1)
})



if(interactive()) setwd("../../")


