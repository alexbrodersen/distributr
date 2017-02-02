context("travis-sge")

test_that("sge works", {
  skip_on_cran()
  plan <- sge_test()

})

test_that("on_sge", {
  skip_on_cran()
  clean()
  test_it <- function(x){on_sge()}
  plan <- gapply(test_it, x=1, .eval=FALSE)
  setup(plan)
  submit()
  res <- collect(plan)
  expect_true(res[[1]])
})
