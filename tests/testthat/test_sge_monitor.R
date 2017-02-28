context("sge monitor")

if(interactive()) setwd("tests/testthat/")
source("../sge_usage_strings.R") # loads str1, str2, str3 for testing

test_that("parse_qstat works with tasks", {
  df <- parse_qstat(stat1)$run
  expect_equal(ncol(df), 10)
})
test_that("parse_qstat works no tasks", {
  df <- parse_qstat(stat2)$run
  expect_equal(ncol(df), 10)
})

test_that("parse_usage works with tasks", {
  df <- parse_usage(use1)
  expect_equal(nrow(df), 72)
  expect_true(all(df$status == "r"))
  expect_true(all(df$job_id == 744755))
})

test_that("parse_usage works with N/A maxvmem", {
  x <- parse_usage(use2)
  expect_equal(nrow(x), 24)
  expect_true(all(is.na(x$maxvmem)))
  expect_true(all(is.na(x$vmem)))
  expect_true(all(x$mem == 0))
  expect_true(all(x$wallclock == 0))
  expect_true(all(x$cpu == 0))
})

test_that("parse_usage filters exited jobs", {
  x <- parse_usage(use3)
  expect_true(nrow(x) == 19)
})

test_that("parse_usage returns NULL with no running jobs", {
  x <- parse_usage(use4)
  expect_null(x)
})

test_that("parse_usage works with no tasks", {
  x <- parse_usage(use5)
  expect_true(nrow(x) == 1)
})


if(interactive()) setwd("../../")


