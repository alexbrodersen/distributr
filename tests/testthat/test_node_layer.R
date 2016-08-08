## test grid

#library(ggplot2)
context("test node layer")

ff <- function(a, b){a + b}
gg <- function(x, arg1){x^2}
hh <- function(x, arg2){-x}

n1 <- node(ff, a=1:3)
expect_true(is.node(n1))

l1 <- layer(.dgraph=NULL, node(ff, a=1:3, b=1:3), node(ff, a=4:5, b=4:5), .id=1)
is.dgraph(l1)

l1 <- distributr:::assign_node_ids(l1, start = 0)

l2 <- layer(node(hh, arg2=11:14), node(gg, arg1=11:13), .id = 2)

l1 <- layer(node(ff, a=1:3, b=1:3), node(ff, a=4:5, b=4:5), .id=1)
l2 <- layer(node(hh, arg2=1), node(gg, arg1=1), .id=2)

o <- layer(node(ff, a=1:3, b=1:3), node(ff, a=4:5, b=4:5)) %>%
  layer(node(hh, arg2=1), node(gg, arg1=1))

o2 <- layer(node(ff, a=1:3, b=1:3), node(ff, a=4:5, b=4:5), .id=1) +
  layer(node(hh, arg2=1), node(gg, arg1=1), .id=2) +
  dcontrol() + reps(500)

expect_true(get_node(o2, 2)$.id == 2)
expect_true(get_node(o2, 3)$.id == 3)





