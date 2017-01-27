context("summary gapply")


do.one <- function(a=1,b=2){data.frame(sum=a+b,sub=a-b)}
out <- gapply(do.one,.reps=3, a=1,b=2,.verbose=0)
x <- grid_apply(do.one,.reps=3, a=1,b=2,.verbose=0)

test_that("summary", {
  msg <- capture.output(expect_output(summary(out), "Estimated time"))
  msg <- capture.output(expect_output(summary(out), "Number of conditions:  1"))
})

