## test grid


ff <- function(a, b){a + b}
gg <- function(x){x^2}
hh <- function(x){-x}

grid(ff, a=1:3) + grid(gg)

distributr:::add_grid(grid(ff, a=1:3), grid(gg))

o <- grid(hh, .level=2) +
  grid(gg, .level=1) +
  grid(ff, a=1:3, b=1:3, .level=0) +
  reps(1000) +
  tidy(.level=1) +
  local()
