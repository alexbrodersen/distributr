context("travis-sge")

test_that("sge works", {
  skip_on_cran()
  plan <- sge_test()

})
