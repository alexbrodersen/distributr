## Interactive setting up of simulation from gapply test object

#' Setup SGE jobs
#'
#' In a given directory, writes the argument grid given from \code{grid_apply(.f, ..., .eval=FALSE)},
#' an Rscript to run \code{.f} on one set of arguments,
#' a submission script to run \code{.f} on combination of arguments,
#' and directories to store results and job log files.
#'
#' @param object object from \code{grid_apply} or \code{gapply} with \code{.eval=FALSE}
#' @param .seed An integer or \code{NULL} (default, no seeds are set automatically). If given, controls
#' RNG using L'ecuyer-CMRG as in \code{parallel} by saving and accessing unique seeds in \code{seeds.Rdata}
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
#' @param .R.version name of R version. Possible values include any in \code{module avail}. Default is 3.2.5.
#' @param .email.options one or more characters from "bea" meaning email when "job Begins", "job Ends", and "job Aborts". Default is "a".
#' @param .email.addr email address
#' @param .shell shell to use. Default is 'bash'
#' @param ... unused
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
setup <- function(object, ...){
  UseMethod("setup")
}

#' @export
#' @describeIn setup Setup sge files from \code{gapply, grid_apply}
setup.gresults <- function(object,
                  .seed=NULL,
                  .dir=getwd(),
                  .reps=1,
                  .mc.cores=1,
                  .verbose=1,
                  .queue="long",
                  .script.name="doone.R",
                  .job.name="distributr",
                  .out.dir="SGE_Output",
                  .R.version="3.2.5",
                  .email.options="a",
                  .email.addr=NULL,
                  .shell="bash",
                  ...){
  arg_grid <- attr(object,"arg_grid")
  arg_grid$.sge_id <- 1:nrow(arg_grid)
  .dir <- paste0(.dir, "/")

  # 2017-01-25 Previously, I added automatic rep chunking. However, it was removed
  #  when I decided to add .sge_id as an actual variable in arg_grid in 'setup'.
  #  Maintaining auto chunking and .sge_id is tricky because 'tidy' needs to work
  #  with both the non-chunked arg_grid from 'grid_apply',
  #  and the chunked arg_grid from 'setup'.
  #  Dealing with it has wasted so much of my time, and there is no easy solution.
  #
  #  Chunking of replications can be handled easily by the user by just including 'chunk' as a
  #  variable. .reps becomes .reps per chunk automatically.
  #  'chunk' can then just be removed in the analysis.
  #  The results are transparent, rather than magical (my preference).
  #
  #  Chunking of conditions (running multiple rows with each submission) can
  #  be achieved by nesting a gapply call within another. The submitted
  #  conditions are in the outer call, while the other conditions are run in the
  #  inner call.

  #  Replications are still the slowest varying factor, to prioritize running more
  #  conditions rather than replications.


  # initialize sge_id if there is none
  if(is.null(arg_grid$.sge_id)){
    arg_grid$.sge_id <- 1:nrow(arg_grid)
  }

  .f <- attr(object,".f")
  attr(arg_grid, ".args") <- attr(object, ".args")

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
               R.version = .R.version,
               shell = .shell)

  grid_name <- paste0(.dir, "arg_grid.Rdata")
  res <- check_overwrite(object=object, .dir=.dir)
  if(res == 1){
    cat("writing arg_grid.Rdata", fill=T)
    saveRDS(arg_grid, file=grid_name)
  }

  if(!is.null(.seed)){
    write_seeds(.sge_ids = arg_grid$.sge_id, dir = .dir, seed=.seed)
  }

  write_doone(.f=.f, dir=.dir, seed=.seed, reps=.reps, mc.cores=.mc.cores,
              verbose=.verbose, script.name=.script.name)

  attr(object, "arg_grid") <- arg_grid
  attr(object, ".reps") <- .reps
  return(object)
}

write_submit <- function(dir, script.name="doone.R", mc.cores=1, tasks=1, queue="long",
                         job.name="distributr", out.dir="SGE_Output", email="a",
                         email.addr=NULL, shell="csh", R.version="3.2.5"){

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
      "module load R/", R.version, "\n",
      "Rscript ", script.name, " $SGE_TASK_ID $NSLOTS \n")
  } else {
    submit <- paste0(
      "#!/bin/", shell, " \n",
      "#$ -pe smp ",min(mc.cores), "-", max(mc.cores), "\n",
      "#$ -q ", queue, "\n",
      "#$ -N ", job.name, "\n",
      "#$ -t ", tasks, "\n",
      "#$ -o ", out.dir, " \n\n",
      "module load R/", R.version, "\n",
      "Rscript ", script.name, " $SGE_TASK_ID $NSLOTS \n")
  }
  cat(submit, file=paste0(dir, "submit"))
}

write_doone <- function(.f, dir, seed, reps=1, mc.cores=1, verbose=1, script.name="doone.R"){

  fstr <- paste0(".f <- ", paste0(deparse(eval(.f), control="all"), collapse="\n"))

  if(!is.null(seed)){
    seed_load <- paste0("\n
  RNGkind(\"L'Ecuyer-CMRG\")
  seeds <- readRDS('seeds.Rdata')
  .Random.seed <- seeds[[params$.sge_id]]\n")
  } else {
    seed_load <- paste0("\n")
  }

  script <- paste0(fstr,"
  suppressMessages(library(distributr))
  args <- as.numeric(commandArgs(trailingOnly=TRUE))
  sge_id <- args[1]
  ncores <- args[2]
  reps <- ", reps,"
  arg_grid <- readRDS('arg_grid.Rdata')
  .args <- attr(arg_grid, '.args')
  params <- arg_grid[sge_id,]",
  seed_load,"
  rep.id <- 1:reps
  params$.sge_id <- NULL  # special variable not in f
  start <- proc.time()
  res.l <- do.rep(wrapWE(.f), as.list(params), .reps=reps, .args=.args,
    .rep.cores=ncores, .verbose=", verbose," )
  end <- proc.time()
  dir <- paste0('results/')
  attr(res.l, \"time\") <- end - start
  fn <- paste0(dir, sge_id,'.Rdata')
  saveRDS(res.l, file=fn) \n")

  cat(script, file=paste0(dir, script.name))
}

#' @importFrom parallel nextRNGStream
write_seeds <- function(dir, .sge_ids, seed){
  fn <- paste0(dir, "seeds.Rdata")
  RNGkind("L'Ecuyer-CMRG")
  set.seed(seed)
  s <- .Random.seed
  seeds <- vector(mode = "list", length = max(.sge_ids))
  for(i in .sge_ids){
    seeds[[i]] <- parallel::nextRNGStream(s)
    s <- seeds[[i]]
  }
  saveRDS(seeds, file=fn)
  cat("writing seeds.Rdata", fill=T)
}

#' Collect completed results files from gresults
#'
#' Provides arguments to return a sample of the results files of a given size,
#' or to return results from files matching a regular expression.
#'
#' @param x object from \code{grid_apply}
#' @param filter a quoted string or formula filtering jobs in \code{arg_grid} as in \code{filter}.
#' Results for jobs matching filter are returned.
#' If \code{NULL} (default), all results are returned.
#' @param regex regular expression  matching files in \code{results/}.
#' If \code{NULL} (default), all results are returned.
#' @param sample the number of files in \code{results/} to sample from.
#' If \code{NULL} (default), all results are returned.
#' @param dir directory
#' @param ... unused
#' @details \code{filter, regex} and \code{sample} are applied to the available results in order.
#' For example, results are filtered first, a regex is applied, then a sample is taken.
#' @export
#' @importFrom gtools mixedsort
#' @importFrom tidyr gather
#' @importFrom dplyr collect filter_
collect.gresults <- function(x, filter=NULL, regex=NULL, sample=NULL, dir=getwd(), ...){
  dir <- paste0(dir, "/")
  arg_grid <- readRDS(paste0(dir, "arg_grid.Rdata"))

  rdir <- paste0(dir, "results/")
  conds.files <- gtools::mixedsort(paste0(rdir,list.files(rdir)))
  if(!is.null(filter)){
    collected_ids <- as.numeric(gsub(".Rdata", "", gsub(paste0(dir, "results/"), "", conds.files)))
    grid_filter <- filter_(arg_grid, .dots=filter)
    conds.files <- conds.files[collected_ids %in% grid_filter$.sge_id]
  }
  if(!is.null(regex)){
    conds.files <- grep(conds.files, pattern = regex, value=TRUE)
  }
  if(!is.null(sample)){
    conds.files <- conds.files[sample(1:length(conds.files), size = sample, replace = FALSE)]
  }
  cond.l <- list()           # list of the results from each condition
  for(i in 1:length(conds.files)){
      fn <- paste0(conds.files[i])
      res.l <- readRDS(fn)
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
#' @importFrom dplyr bind_rows
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
#' @importFrom dplyr filter
filter_jobs <- function(object, ...,
                        .mc.cores=1,
                        .dir= getwd(),
                        .queue="long",
                        .script.name="doone.R",
                        .job.name="distributr",
                        .out.dir="SGE_Output",
                        .R.version="3.2.5",
                        .email.options="a",
                        .email.addr=NULL,
                        .shell="bash"){
  .dir <- paste0(.dir, "/")
  arg_grid <- jobs(object)
  if(is.null(arg_grid$.sge_id)){
    arg_grid$.sge_id <- 1:nrow(arg_grid)
  }
  arg_grid <- filter(arg_grid, ...)
  tasks <- unlist(arg_grid$.sge_id, use.names = FALSE)

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
               R.version = .R.version,
               shell = .shell)

  attr(object, "arg_grid") <- arg_grid
  invisible(object)
}

#' Return running jobs
#' @export
qst <- function(){
  system("qstat -u $USER", intern=TRUE)
}


#' Run and \code{cat} a call to \code{system}
#' @param cmd string of the command to run
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

#' Removes all files and folders created by \code{setup} and their contents
#' @param dir project directory name followed by 'slash'
#' @details Removes \code{results/*}, \code{SGE_Output/*}, \code{arg_grid.Rdata},
#' \code{submit}, \code{seeds.Rdata}, and \code{doone.R}.
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
    fn <- paste0(dir, "seeds.Rdata")
    if(file.exists(fn)){
      cmd <- paste0("rm ", dir, "seeds.Rdata")
      mysys(cmd)
    }
  }
}

#' sge_test
#' Simple test SGE configuration
#' @param dir directory in which to run test
#' @export
sge_test <- function(dir=getwd()){
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

#' Runs a job on the head node
#' @param .sge_id job id (row of \code{jobs(object)})
#' @param .mc.cores number of cores to use
#' @param .script.name name of the script given in \code{setup}
#' @details
#' Runs \code{Rscript doone.R x y} at the command line where \code{x} is \code{.sge_id},
#' and \code{y} is \code{.mc.cores}.
#'
#' Note that long running jobs claiming many cores may be killed by
#' the administrators. This should only be used for short tests, longer tests
#' should be submitted to the cluster as usual.
#'
#' @seealso \code{submit}, \code{filter}
#' @export
test_job <- function(.sge_id=1, .mc.cores=1, .script.name="doone.R"){
  cmd <- paste0("Rscript ", .script.name, " ", .sge_id, " ", .mc.cores)
  mysys(cmd)
}



