## Interactive setting up of simulation from gapply test object

#' Setup SGE jobs
#'
#' In a given directory, writes the argument grid given from \code{grid_apply(.f, ..., .eval=FALSE)},
#' an Rscript to run \code{.f} on one set of arguments,
#' a submission script to run \code{.f} on combination of arguments,
#' and directories to store results and job log files.
#'
#' @param object object from \code{grid_apply} or \code{gapply} with \code{.eval=FALSE}
#' @param .dir directory name relative to the current working directory
#' @param .reps total number of replications for each condition
#' @param .mc.cores number of cores used to run replications in parallel (can be a range)
#' @param .verbose verbose level: \code{1} prints '.' for each replication,
#' \code{2} prints '.' on completion and prints the current arguments,
#' \code{3} prints the current arguments and results
#' @param .script.name name of script (default \code{doone.R})
#' @param .queue name of queue
#' @param .job.name name of job
#' @param .out.dir name of directory in which to put SGE output files.
#' @param .email.options one or more characters from "bea" meaning email when "job Begins", "job Ends", and "job Aborts". Default is "a".
#' @param .email.addr email address
#' @param .shell shell to use. Default is 'bash'
#' @return Invisibly, the original object with argument grid modified to append a
#' column \code{$.sge_id} assigning each row to a unique job id.
#'
#' As side effects, the function writes the following objects to \code{.dir}:
#' \item{arg_grid.Rdata}{Data frame containing the argument grid,
#' appended with a column \code{sge_id} corresponding to the task id of each row}
#' \item{doone.R}{Script to run one job, or one row from \code{arg_grid}}
#' \item{submit}{Submission script specifying a task array over the grid of parameters
#' in (all rows of) arg_grid.Rdata}
#' \item{results/}{Folder to store results. Each file is \code{1.Rdata}, \code{2.Rdata}, ...
#' corresponding to the task id (row in \code{arg_grid})}
#' \item{SGE_Output/}{ Folder for output from SGE}
#'
#' @details
#' Long running \code{grid_apply} computations can be easily run in parallel on
#' SGE using array tasks. Each row in the argument grid given by \code{expand_grid(...)}
#' is mapped to a unique task id, which is run on a separate node.
#'  \code{setup()} makes this easy by writing
#' the argument grid (\code{arg_grid.Rdata}), R script to run one combination of arguments, a submission
#' script assigning all rows to a unique task id, and folders to store results in
#' a given directory. Jobs are submitted to the scheduler by running \code{qsub submit}
#' at the prompt, or by running \code{submit()} within R.
#'
#' The argument grid (\code{arg_grid}) is saved to \code{.dir} as \code{arg_grid.Rdata}.
#' It contains the columns of \code{expand.grid(...)} from \code{grid_apply}.
#' A column \code{$.sge_id} is appended that assigns each row a unique job id.
#'
#' A simple R script (\code{doone.R}) is provided that runs \code{.f} on one row
#' of \code{arg_grid}. Running \code{doone.R} at the command line exactly replicates
#' how the script will be run on each node.
#'
#' A file (\code{submit}) is also written, which specifies a task array for \code{qsub}
#' for all jobs in \code{arg_grid}. It can be submitted to the queue by running
#' \code{qsub submit} at the command line. Job status can be monitored with \code{qstat}.
#' Various email
#'
#' Results are stored in \code{results/}, as \code{$SGE_TASK_ID.Rdata} where
#' \code{SGE_TASK_ID} is the array task corresponding to a unique row in \code{arg_grid}.
#' It is sometimes convenient to access this variable within \code{.f}, which can
#' be done by \code{Sys.getenv("SGE_TASK_ID")}. This might be used to set seeds
#' or to cache intermediate results for example.
#'
#' The function \code{.f} can be run multiple times for every row in \code{arg_grid}
#' by setting \code{.reps > 1}. These replications can be run in parallel using
#' \code{mclapply} by setting \code{.mc.cores > 1}. To decrease waiting times in the queue,
#' \code{mc.cores} can be given a range (e.g. \code{mc.cores = c(1, 8)}), and the job will
#' be submitted when a given set of cores in that range is available. To access the
#' number of cores given to each job, use \code{Sys.getenv("NSLOTS")}.
#'
#' It is easy to corrupt \code{arg_grid.Rdata} by running \code{setup} on different
#' sets of arguments, making future merges of results with arguments based on
#' \code{.sge_id} invalid. If \code{arg_grid.Rdata} already exists, \code{setup}
#' verifies with user input that an overwrite is intended.
#' @examples
#' \dontrun{
#' do.one <- function(a, b){c(sum=a+b, sub=a-b)}
#' plan <- grid_apply(do.one, a=1:5, b=3, .eval=FALSE)
#' jobs(plan)  # shows the original argument grid
#' plan <- setup(plan, .reps=5, .mc.cores=c(1, 5))
#' jobs(plan)  # modified with a column showing unique job ids
#' }
#' @export
#' @seealso \link{grid_apply} to define the grid, \link{jobs} to see the grid,
#' \link{collect} to collect completed results, and \link{tidy} to merge
#' completed results with the argument grid.
setup <- function(x, ...){
  UseMethod("setup")
}

#' Setup sge files from gresults
#' @export
#' @describeIn setup
setup.gresults <- function(object,
                  .dir=getwd(),
                  .reps=1,
                  .mc.cores=1,
                  .verbose=1,
                  .queue="long",
                  .script.name="doone.R",
                  .job.name="distributr",
                  .out.dir="SGE_Output",
                  .email.options="a",
                  .email.addr=NULL,
                  .shell="bash"){
  arg_grid <- attr(object,"arg_grid")
  arg_grid$.sge_id <- 1:nrow(arg_grid)
  .dir <- paste0(.dir, "/")

  # 2016-12-09 Previously, I added automatic rep chunking. However, it was removed
  #  when I decided to add .sge_id as an actual variable in arg_grid in 'setup'.
  #  Maintaining auto chunking and .sge_id is tricky because 'tidy' needs to work
  #  with both the non-chunked arg_grid from 'grid_apply',
  #  and the chunked arg_grid from 'setup'.
  #  Dealing with it has wasted so much of my time, and there is no easy solution.
  #
  #  The cleanest solution would be to never evaluate and store the full arg_grid,
  # and only subset arg_grid rows when necessary.
  #
  #  Chunking can be handled easily by the user by just including 'chunk' as a
  #  variable. Everything would work, and .reps becomes .reps per chunk automatically.
  #  'chunk' can then just be removed in the analysis.
  #  The results are transparent, rather than magical (my preference).
  #  The only drawback is that a unique
  #  rep id must be obtained by the user if needed, but I have never needed this!

  #  Replications are still the slowest varying factor, to prioritize running more
  #  conditions rather than replications.

  #  Here is the original chunking code:
  #chunk.grid <- arg_grid[rep(1:nrow(arg_grid), times=.chunks),,drop=F]
  #chunk.grid$.chunk <- rep(1:.chunks, each=nrow(arg_grid))
  #chunk.grid$.sge_id <- 1:nrow(chunk.grid)
  #reps.per.chunk <- ceiling(.reps/.chunks)

  # initialize sge_id if there is none
  if(is.null(arg_grid$.sge_id)){
    arg_grid$.sge_id <- 1:nrow(arg_grid)
  }

  .f <- attr(object,".f")

  cmd <- paste0("mkdir -p ", .dir, "results")
  mysys(cmd)
  cmd <- paste0("mkdir -p ", .dir, "SGE_Output")
  mysys(cmd)

  write_submit(.dir, script.name=.script.name, mc.cores=.mc.cores,
               tasks=paste0("1:", nrow(arg_grid)),
               job.name=.job.name,
               out.dir = .out.dir,
               email = .email.options,
               email.addr = .email.addr,
               shell = .shell)

  grid_name <- paste0(.dir, "arg_grid.Rdata")
  res <- check_overwrite(object=object, .dir=.dir)
  if(res == 1){
    cat("writing arg_grid.Rdata", fill=T)
    save(arg_grid, file=grid_name)
  }

  write_doone(.f=.f, dir=.dir, reps=.reps, mc.cores=.mc.cores, verbose=.verbose, script.name=.script.name)

  attr(object, "arg_grid") <- arg_grid
  attr(object, ".reps") <- .reps
  return(object)
}

write_submit <- function(dir, script.name="doone.R", mc.cores=1, tasks=1, queue="long",
                         job.name="distributr", out.dir="SGE_Output", email="a",
                         email.addr=NULL, shell="csh"){

  cmd <- paste0("touch ", dir, "submit")
  mysys(cmd)
  if(!is.null(email.addr)){
    submit <- paste0(
      "#!/bin/", shell, " \n",
      "#$ -M ", email.addr, "\n",
      "#$ -m ", email, "\n",
      "#$ -pe smp ",min(mc.cores), "-", max(mc.cores), "\n",
      "#$ -q ", queue, "\n",
      "#$ -N ", job.name, "\n",
      "#$ -t ", tasks, "\n",
      "#$ -o ", out.dir, " \n\n",
      "Rscript ", script.name, " $SGE_TASK_ID $NSLOTS \n")
  } else {
    submit <- paste0(
      "#!/bin/", shell, " \n",
      "#$ -pe smp ",min(mc.cores), "-", max(mc.cores), "\n",
      "#$ -q ", queue, "\n",
      "#$ -N ", job.name, "\n",
      "#$ -t ", tasks, "\n",
      "#$ -o ", out.dir, " \n\n",
      "Rscript ", script.name, " $SGE_TASK_ID $NSLOTS \n")
  }
  cat(submit, file=paste0(dir, "submit"))
}

write_doone <- function(.f, dir, reps=1, mc.cores=1, verbose=1, script.name="doone.R"){
  fstr <- paste0(".f <- ", paste0(deparse(eval(.f), control="all"),collapse="\n"))
  temp <- paste0(fstr,"
  suppressMessages(library(distributr))
  args <- as.numeric(commandArgs(trailingOnly=TRUE))
  cond <- args[1]
  ncores <- args[2]
  reps <- ", reps," # this is reps per chunk
  load('arg_grid.Rdata')
  params <- arg_grid[cond,]
  #rep.id <- (reps*(params$.chunk-1)+1):(reps*params$.chunk)
  rep.id <- 1:reps
  #params$.chunk <- NULL    # because f doesn't take chunk usually
  params$.sge_id <- NULL  # special variable not in f
  start <- proc.time()
  res.l <- do.rep(wrapWE(.f), as.list(params), .reps=reps, .rep.cores=ncores, .verbose=", verbose," )
  end <- proc.time()
  dir <- paste0('results/')
  attr(res.l, \"time\") <- end - start
  fn <- paste0(dir, cond,'.Rdata')
  save(res.l, file=fn) \n")

  cat(temp, file=paste0(dir, script.name))
}

#' Collect completed results files from gresults
#'
#' @export
#' @importFrom gtools mixedsort
#' @importFrom tidyr gather
#' @importFrom dplyr collect
collect.gresults <- function(object, dir=getwd()){
  dir <- paste0(dir, "/")
  load(paste0(dir, "arg_grid.Rdata"))

  rdir <- paste0(dir, "results/")
  conds.files <- gtools::mixedsort(paste0(rdir,list.files(rdir)))
  cond.l <- list()           # list of the results from each condition
  for(i in 1:length(conds.files)){
      fn <- paste0(conds.files[i])
      load(fn)
      cond.l[[i]] <- res.l
  }
  reps <- lengths(cond.l)

  cond.l <- unlist(cond.l, recursive=F)

  completed.idx <- as.numeric(gsub(".Rdata", "", basename(conds.files))) # completed conditions
  completed.grid <- arg_grid[completed.idx,]

  err <- lapply(cond.l, function(r){attr(r, "err")})
  err.id <-  which(unlist(lapply(err, function(x){!is.null(x)})))
  err.list <- err[err.id]
  names(err.list) <- err.id

  warn <- lapply(cond.l, function(r){attr(r, "warn")})
  warn.id <- which(unlist(lapply(warn, function(x){!is.null(x)})))
  warn.list <- warn[warn.id]
  names(warn.list) <- warn.id

  times <- lapply(cond.l, function(r){attr(r, "time")})

  res <- cond.l
  class(res) <- c(class(res), "gresults")
  attr(res, "arg_grid") <- completed.grid
  attr(res, ".reps") <- reps
  attr(res, "err") <- err.list
  attr(res, "warn") <- warn.list

  return(res)
}

#' Add jobs (or rows) to argument grid
#' @param object grid_apply object
#' @param ... key value pairs for \code{.f} in \code{grid_apply}
#' @return \code{grid_apply} object with updated
#' @details If all original keys are not in ..., the values of these arguments are set
#' to \code{NA}
#' @export
add_jobs <- function(object, ...){
  arg_grid <- attr(object, "arg_grid")
  new_grid <- expand.grid(...)

  if(!is.null(arg_grid$.sge_id)){
    last <- max(arg_grid$.sge_id)
    new_grid$.sge_id <- (last+1):(last+nrow(new_grid))
    arg_grid <- bind_rows(arg_grid, new_grid)
  } else {
    arg_grid <- bind_rows(arg_grid, new_grid)
    arg_grid$.sge_id <- 1:nrow(arg_grid)
  }

  attr(object, "arg_grid") <- arg_grid
  return(object)
}

add_rows <- add_jobs


#' Filter a subset of jobs (rows) from argument grid and modify submission script
#'
#' @export
#' @param ... arguments to \code{select}
#' @inheritParams setup
filter_jobs <- function(object, ...,
                        .mc.cores=1,
                        .dir= getwd(),
                        .queue="long",
                        .script.name="doone.R",
                        .job.name="distributr",
                        .out.dir="SGE_Output",
                        .email.options="a",
                        .email.addr=NULL,
                        .shell="bash"){
  .dir <- paste0(.dir, "/")
  arg_grid <- jobs(object)
  if(is.null(arg_grid$.sge_id)){
    arg_grid$.sge_id <- 1:nrow(arg_grid)
  }
  arg_grid <- filter(arg_grid, ...)
  tasks <- select(arg_grid, .sge_id) %>% unlist(., use.names = F)

  if(all(tasks == tasks[1]:tasks[length(tasks)])){
    tasks <- paste0(tasks[1], ":", tasks[length(tasks)])
  } else {
    tasks <- paste0(tasks, collapse=", ")
  }
  print(head(arg_grid))

  write_submit(.dir, script.name=.script.name,
               mc.cores=.mc.cores,
               tasks=tasks,
               job.name=.job.name,
               out.dir = .out.dir,
               email = .email.options,
               email.addr = .email.addr,
               shell = .shell)

  attr(object, "arg_grid") <- arg_grid
  invisible(object)
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


#' Submit jobs to SGE
#' @param dir directory of submission script
#' @export
submit <- function(dir=getwd()){
  wd <- getwd()
  setwd(dir)
  cmd <- paste0("qsub submit")
  mysys(cmd)
  setwd(wd)
}

#' Cleans results
#' @param dir project directory name followed by 'slash'
#' @export
clean <- function(dir=getwd()){
  dir <- paste0(dir, "/")
  rdir <- paste0(dir, "results/")
  sdir <- paste0(dir, "SGE_Output/")
  arg_name <- paste0(dir, "arg_grid.Rdata")
  if(file.exists(rdir)){
    cmd <- paste0("rm -rf ", rdir, "")
    mysys(cmd)
    cmd <- paste0("rm -rf ", sdir, "")
    mysys(cmd)
    cmd <- paste0(paste0("rm ", arg_name))
    mysys(cmd)
    cmd <- paste0("rm ", dir, "submit")
    mysys(cmd)
    cmd <- paste0("rm ", dir, "doone.R")
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

#' Returns the parameter grid from grid_apply
#' @param object object from grid_apply
#' @export
jobs <- function(object){
 attr(object, "arg_grid")
}



