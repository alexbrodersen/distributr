f <- function(a=1,b=2){
  if(a==1) stop("asdf")
  a
}
  library(patr1ckm)
  args <- as.numeric(commandArgs(trailingOnly=TRUE))
  cond <- args[1]
  reps <- 2 # this is reps per chunk
  load('param_grid.Rdata')
  params <- param.grid[cond,]
  rep.id <- (reps*(params$chunk-1)+1):(reps*params$chunk)
  params$chunk <- NULL # because f doesn't take chunk usually
  res.l <- do.rep(wrapWE(f), as.list(params), .reps=reps, .rep.cores=1, .verbose=2 )
  dir <- paste0('results/')
  fn <- paste0(dir, cond,'.Rdata')
  save(res.l, file=fn)