
fdir <- "tests/testthat/tests/tmp_nl"

system(paste0("rm -rf ", fdir))

ff <- function(a, b){a + b}
gg <- function(x, arg1){x^2}
hh <- function(x, arg2){-x}

o <- layer(node(ff, a=1:3, b=1:3), node(ff, a=4:5, b=4:5)) %>%
  layer(node(hh, arg2=1), node(gg, arg1=1)) %>%
  dcontrol() %>% reps(5)

setup.dgraph(o, dir=fdir)

setwd("tests/tmp_nl")
for(i in 1:39) {
  cmd <- paste0("Rscript doone.R ", i)
  system(cmd)
}
