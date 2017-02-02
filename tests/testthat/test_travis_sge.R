context("travis-sge")

test_that("sge works", {
  skip_on_cran()
  plan <- sge_test()

})

test_that("sge works", {
  skip_on_cran()
  cat("PRINT THIS", fill=T)
  test_it <- function(x){on_sge()}
  plan <- gapply(test_it, x=1)
  setup(plan)
  submit(plan)
  res <- collect(plan)
  expect_true(res[[1]])
})
