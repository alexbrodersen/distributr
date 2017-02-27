context("sge monitor")

if(interactive()) setwd("tests/testthat/")
source("../sge_usage_strings.R") # loads str1, str2, str3 for testing

test_that("sge qstat works", {
  x <- c("job-ID     prior   name       user         state submit/start at     queue                          jclass                         slots ja-task-ID ",
    "------------------------------------------------------------------------------------------------------------------------------------------------",
    "    740473 0.60142 sleep_ss_l pmille13     r     02/08/2017 15:38:00 long@q16copt036.crc.nd.edu                                       24 1",
    "    740473 0.60142 sleep_ss_l pmille13     r     02/08/2017 15:38:00 long@q16copt036.crc.nd.edu                                       24 2",
    "    740473 0.60142 sleep_ss_l pmille13     r     02/08/2017 15:38:00 long@q16copt027.crc.nd.edu                                       24 3",
    "    740473 0.60142 sleep_ss_l pmille13     r     02/08/2017 15:38:00 long@q16copt060.crc.nd.edu                                       24 5",
    "    743563 0.50534 distributr pmille13     r     02/11/2017 19:05:02 long@d12chas447.crc.nd.edu                                        1 12103",
    "    743563 0.50534 distributr pmille13     r     02/11/2017 19:09:16 long@d12chas366.crc.nd.edu                                        1 12104",
    "    743563 0.00000 distributr pmille13     qw    02/11/2017 14:10:31                                                                   1 12105-24000:1"
  )
  df <- parse_qstat(x)$run
  expect_equal(ncol(df), 10)
})

test_that("sge parse qstat -j works", {
  df <- parse_usage(str1)
  expect_equal(nrow(df), 72)
  expect_true(all(df$status == "r"))
  expect_true(all(df$job_id == 744755))
})

test_that("sge parse_usage works with N/A maxvmem", {
  x <- parse_usage(str2)
  expect_equal(nrow(x), 24)
  expect_true(all(is.na(x$maxvmem)))
  expect_true(all(is.na(x$vmem)))
  expect_true(all(x$mem == 0))
  expect_true(all(x$wallclock == 0))
  expect_true(all(x$cpu == 0))
})

test_that("exited jobs filtered", {
  x <- parse_usage(str3)
  expect_true(nrow(x) == 19)
})

if(interactive()) setwd("../../")


