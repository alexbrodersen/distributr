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
expect_true(!is.null(attr(o, ".dcontrol")))

## Test classes
expect_equal(sapply(o, class), c("layer", "layer"))
for(i in 1:2) expect_equal(sapply(o[[i]], class), c("node", "node"))

## Test ids
expect_equal(sapply(o[[1]], function(node){node$.id}), c(1,2))
expect_equal(sapply(o[[2]], function(node){node$.id}), c(3, 4))
expect_equal(sapply(o, function(l){attr(l, ".id")}), c(1, 2))

## Test get_node
for(i in 1:4) expect_true(get_node(o, i)$.id == i)

o <- dcontrol(o)
expect_true(!is.null(attr(o, ".dcontrol")))
o <- reps(o, 500)
expect_equal(attr(o, ".dcontrol")$reps, 500)

o <- layer(node(ff, a=1:3, b=1:3), node(ff, a=4:5, b=4:5)) %>%
  layer(node(hh, arg2=1), node(gg, arg1=1)) %>%
  dcontrol() %>% reps(5)
expect_equal(attr(o, ".dcontrol")$reps, 5)


system("mkdir -p tests/tmp_nl")
system("rm -rf tests/tmp_nl/*")
setup.dgraph(o, dir="tests/tmp_nl")



