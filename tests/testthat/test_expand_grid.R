x1 <- list(data.frame(a=1:5, b=1:5), data.frame(a=1:5, b=1:5))
x <- expand_grid(x1, d=1:3, f=c(TRUE, FALSE))


x <- expand.grid(a=1:2, b=2, KEEP.OUT.ATTRS=FALSE)
y <- as.data.frame(expand_grid(a=1:2, b=2))
expect_equal(x, y)
