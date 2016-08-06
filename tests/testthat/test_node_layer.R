## test grid

#library(ggplot2)
context("test node layer +")

ff <- function(a, b){a + b}
gg <- function(x, arg1){x^2}
hh <- function(x, arg2){-x}

n1 <- node(ff, a=1:3)
expect_true(is.node(n1))

l1 <- layer(node(ff, a=1:3, b=1:3), node(ff, a=4:5, b=4:5), .id=1)
expect_true(is.layer(l1))

l1 <- distributr:::assign_node_ids(l1, start = 0)
distributr:::layer_apply(l1, select = ".id")
expect_equal(as.numeric(distributr:::layer_apply(l1, select = ".id")), 1:2)

l2 <- layer(node(hh, arg2=11:14), node(gg, arg1=11:13), .id = 2)
l2 <- distributr:::assign_node_ids(l2, start = max(layer_apply(l1, ".id")))
expect_equal(as.numeric(distributr:::layer_apply(l2, select = ".id")), 3:4)

g1 <- distributr:::layer_to_graph(l1)
g2 <- distributr:::layer_to_graph(l2, g1)

l1 <- layer(node(ff, a=1:3, b=1:3), node(ff, a=4:5, b=4:5), .id=1)
l2 <- layer(node(hh, arg2=1), node(gg, arg1=1), .id=2)

o1 <- layer(node(ff, a=1:3, b=1:3), node(ff, a=4:5, b=4:5), .id=1) +
  layer(node(hh, arg2=1), node(gg, arg1=1), .id=2)

o2 <- layer(node(ff, a=1:3, b=1:3), node(ff, a=4:5, b=4:5), .id=1) +
  layer(node(hh, arg2=1), node(gg, arg1=1), .id=2) +
  doptions(reps = 30)






