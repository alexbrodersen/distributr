context("summary gapply")


do.one <- function(a=1,b=2){data.frame(sum=a+b,sub=a-b)}
gapply(do.one,.reps=3, a=1,b=2,.verbose=0)
(x <- grid_apply(do.one,.reps=3, a=1,b=2,.verbose=0))

test_that("summary", {
  #msg <- capture.output(expect_output(summary(out), "Estimated time"))
  msg <- capture.output(expect_output(summary(out), "Number of conditions:  1"))
  msg <- capture.output(x <- summary(out))
  expect_is(x,"tbl")
  expect_equal(dim(x), c(2,4))
})

do.one <- function(a=1,b=2,d=4){data.frame(sum=a+b+rnorm(1,d),sub=a-b)}
out <- gapply(do.one,.reps=3, a=1:3,b=2,d=4:5, .verbose=0)


## Tests of .fun
#expect_equivalent(as.numeric(summary(out, .fun=max)[1,".fun(value)"]), max(out[1:3,"value"]))
#expect_equivalent(mean(out[1:3,"value"]), as.numeric(summary(out)[1,".fun(value)"]))

## Tests of .key
msg <- capture.output(expect_true(all(summary(out, .key="sum")$key == "sum")))
msg <- capture.output(expect_true(all(summary(out, .key="sub")$key == "sub")))


## Plots
#do.one <- function(a=1,b=2,d=4){data.frame(sum=a+b+rnorm(1,d),sub=a-b)}
#out <- gapply(do.one,.reps=3, a=1:3,b=2,d=4:5, .verbose=0)

#do.one <- function(a=1){data.frame(sum=a+a, sub=a-a)}
#out <- gapply(do.one, .reps=3, a=1:3)
#plot(out)
