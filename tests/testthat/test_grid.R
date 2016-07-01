## test grid

#library(ggplot2)
context("test grid")

ff <- function(a, b){a + b}
gg <- function(x){x^2}
hh <- function(x){-x}

grid(ff, a=1:3) + grid(gg)

distributr:::add_grid(grid(ff, a=1:3), grid(gg))

o2 <- grid(hh, .level=2) +
  grid(gg, .level=1)

expect_true(nrow(summary(o2)) == 2)
expect_is(o2, "ggraph")

o3 <- grid(hh, .level=2) +
  grid(gg, .level=1) +
  grid(ff, a=1:3, b=1:3, .level=0)

expect_true(nrow(summary(o3)) == 3)
expect_is(o3, "ggraph")

o4 <- grid(hh, .level=3) +
  grid(gg, .level=2) +
  grid(ff, a=1:3, b=1:3, .level=1) +
  goption(reps=100, tidy=1, backend="local")

expect_true(any(grepl("goption", sapply(o4, class))))

