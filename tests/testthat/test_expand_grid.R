context("expand_grid_dgraph")

ff <- function(a, b){a + b}
gg <- function(x, arg1){x^2}
hh <- function(x, arg2){-x}

o <- layer(node(ff, a=1:3, b=1:3), node(ff, a=4:5, b=4:5)) %>%
  layer(node(hh, arg2=1), node(gg, arg1=1)) %>%
  dcontrol() %>% reps(5)

param.grid <- expand_grid_dgraph(o)

ans <- dplyr::bind_rows(list(expand.grid(a=1:3, b=1:3, arg2=1),
               expand.grid(a=1:3, b=1:3, arg1=1),
               expand.grid(a=4:5, b=4:5, arg2=1),
               expand.grid(a=4:5, b=4:5, arg1=1)))

expect_equal(param.grid, ans)

context("tidy")

# todo:
# - test tidying from non-last layer

