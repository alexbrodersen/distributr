## Interactive setting up of simulation from gapply test object

#' setup sge simulation
#' @param object gapply object
#' @param dir directory name relative to the current working directory, ends in '/'
#' @param .reps total number of replications for each condition
#' @param .chunks split \code{.reps} across this many nodes (see details)
#' @param .mc.cores number of cores used to run replications in parallel
#' @param .verbose verbose level
#' @param .script.name name of script
#' @param .queue name of queue
#' @param .job.name name of job
#' @param .out.dir name of directory in which to put SGE output files.
#' @param .email.options one or more characters from "bea" meaning email when "job Begins", "job Ends", and "job Aborts". Default is "a".
#' @param .email.addr email address
#' @param .shell shell to use. Default is 'bash'
#' @details
#' The replications performed per chunk is computed as \code{ceiling(.reps/.chunks)}, which
#' will produce more total replications than requested if \code{.reps} is not evenly divisible by \code{.chunks}
#' @export
setup <- function(object, dir=getwd(),  .reps=1, .chunks = 1, .mc.cores=1, .verbose=1,
                  .queue="long",
                  .script.name="doone.R",
                  .job.name="patr1ckm",
                  .out.dir="SGE_Output",
                  .email.options="a",
                  .email.addr="patr1ckm.crc.nd.edu",
                  .shell="bash"){
  arg.grid <- attr(object,"arg.grid")
  dir <- paste0(dir, "/")
  ## Chunk is the slowest varying factor. So adding replications
  ## will be extending the grid within chunk by more chunks, which can then be
  ## mapped onto SGE_TASK_ID. Don't change this unless you found something better design wise.
  chunk.grid <- arg.grid[rep(1:nrow(arg.grid), times=.chunks),,drop=F]
  chunk.grid$chunk <- rep(1:.chunks, each=nrow(arg.grid))
  .f <- attr(object,".f")
  reps.per.chunk <- ceiling(.reps/.chunks)

  cmd <- paste0("mkdir -p ", dir, "results")
  mysys(cmd)
  cmd <- paste0("mkdir -p ", dir, "SGE_Output")
  mysys(cmd)

  write.submit(dir, script.name=.script.name, mc.cores=.mc.cores, tasks=nrow(chunk.grid),
               job.name=.job.name,
               out.dir = .out.dir,
               email = .email.options,
               email.addr = .email.addr,
               shell = .shell)

  arg.grid <- chunk.grid
  attr(arg.grid, "reps") <- reps.per.chunk*.chunks # total actual reps
  attr(arg.grid, "rpc") <- reps.per.chunk # reps per chunk
  save(arg.grid, file=paste0(dir, "arg_grid.Rdata"))

  write.do.one(.f=.f, dir=dir, reps=reps.per.chunk, mc.cores=.mc.cores, verbose=.verbose, script.name=.script.name)
}




#' @export
qst <- function(){
  mysys("qst")
}

#' @export
mysys <- function(cmd){
  cat(cmd,fill=T)
  system(cmd)
}

write.submit <- function(dir, script.name="doone.R", mc.cores=1, tasks=1, queue="long",
                         job.name="patr1ckm", out.dir="SGE_Output", email="a",
                         email.addr="patr1ckm.crc.nd.edu", shell="bash"){
  cmd <- paste0("touch ", dir, "submit")
  mysys(cmd)
  temp <- paste0(
    "#!/bin/", shell, " \n",
    "#$ -M ", email.addr, "\n",
    "#$ -m ", email, "\n",
    "#$ -pe smp ",mc.cores,"\n",
    "#$ -q ", queue, "\n",
    "#$ -N ", job.name, "\n",
    "#$ -t 1:", tasks, "\n",
    "#$ -o ", out.dir, " \n\n",
    "Rscript ", script.name, " $SGE_TASK_ID")
  cat(temp,file=paste0(dir, "submit"))
}

write.do.one <- function(.f, dir, reps=1, mc.cores=1, verbose=1, script.name="doone.R"){
  fstr <- paste0(".f <- ", paste0(deparse(eval(.f), control="all"),collapse="\n"))
  temp <- paste0(fstr,"
  suppressMessages(library(distributr))
  args <- as.numeric(commandArgs(trailingOnly=TRUE))
  cond <- args[1]
  reps <- ", reps," # this is reps per chunk
  load('arg_grid.Rdata')
  params <- arg.grid[cond,]
  rep.id <- (reps*(params$chunk-1)+1):(reps*params$chunk)
  params$chunk <- NULL # because f doesn't take chunk usually
  res.l <- do.rep(wrapWE(.f), as.list(params), .reps=reps, .rep.cores=", mc.cores, ", .verbose=", verbose," )
  dir <- paste0('results/')
  fn <- paste0(dir, cond,'.Rdata')
  save(res.l, file=fn)")

  cat(temp, file=paste0(dir, script.name))
}



#' Submit jobs to SGE
#' @export
submit <- function(dir=getwd()){
  wd <- getwd()
  setwd(dir)
  cmd <- paste0("qsub submit")
  mysys(cmd)
  setwd(wd)
}

#' Collect completed results files
#'
#' @export
#' @importFrom gtools mixedsort
#' @importFrom tidyr gather
collect <- function(dir=getwd()){
  dir <- paste0(dir, "/")
  load(paste0(dir, "arg_grid.Rdata"))
  reps <- attr(arg.grid, "reps") # Since this is from do.rep, will always be of length reps
  rpc <- attr(arg.grid, "rpc")

  rdir <- paste0(dir, "results/")
  conds.files <- gtools::mixedsort(paste0(rdir,list.files(rdir)))
  cond.l <- list()           # list of the results from each condition
  for(i in 1:length(conds.files)){
      fn <- paste0(conds.files[i])
      load(fn)
      cond.l[[i]] <- res.l
  }

  cond.l <- unlist(cond.l, recursive=F)
  cond.idx <- as.numeric(gsub(".Rdata", "", basename(conds.files))) # completed conditions
  cond.grid <- arg.grid[cond.idx,]

  err <- lapply(cond.l, function(r){attr(r, "err")})
  err.id <-  which(unlist(lapply(err, function(x){!is.null(x)})))
  err.list <- err[err.id]
  names(err.list) <- err.id

  warn <- lapply(cond.l, function(r){attr(r, "warn")})
  warn.id <- which(unlist(lapply(warn, function(x){!is.null(x)})))
  warn.list <- warn[warn.id]
  names(warn.list) <- warn.id

  res <- tidy.gresults(cond.l, arg.grid = cond.grid, .reps = reps)
  res$chunk <- NULL
  return(res)
}



#' Cleans results
#' @param dir project directory name followed by 'slash'
#' @export
clean <- function(dir=getwd()){
  dir <- paste0(dir, "/")
  rdir <- paste0(dir, "results/")
  sdir <- paste0(dir, "SGE_Output/")
  if(file.exists(rdir)){
    cmd <- paste0("rm -rf ", rdir, "*")
    mysys(cmd)
    cmd <- paste0("rm -rf ", sdir, "*")
    mysys(cmd)
  }
}

#' sge
#'
#' @export
sge <- function(dir=getwd()){
  f <- function(x,y){
    Sys.sleep(.5)
    stopifnot(x < 5)
    x
  }
  out <- gapply(f, x=3:8, y=1:2, .eval=F)
  setup(out, dir)
  submit(dir)
}

#' Return parameter grid
#'
#' @export
arg.grid <- function(dir=getwd()){
  load(paste0(dir, "/arg_grid.Rdata"))
  return(arg.grid)
}


