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
#' @param .dir directory name relative to the current working directory (no trailing backslash)
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
#' @param ... arguments to methods
#' @return Invisibly, the original object with argument grid modified to append a
#' column \code{$.sge_id} assigning each row to a unique job id.
#'
#' As side effects, the function writes the following objects to \code{.dir}:
#' \item{arg_grid.Rdata}{Data frame containing the argument grid,
#' appended with a column \code{sge_id} corresponding to the task id of each row}
#' \item{doone.R}{Script to run one job, or one row from \code{arg_grid}}
#' \item{submit}{Submission script specifying a task array over the grid of parameters
#' in (all rows of) arg_grid.Rdata}
#' \item{seeds.Rdata}{If \code{.seed} is specified, a list of seeds for each job.}
#' \item{results/}{Folder to store results. Each file is \code{1.Rdata}, \code{2.Rdata}, ...
#' corresponding to the task id (row in \code{arg_grid})}
#' \item{SGE_Output/}{ Folder for output from SGE}
#'
#' @details
#' Long running \code{grid_apply} computations can be easily run in parallel on
#' SGE using array tasks. Each row in the argument grid given by \code{grid_apply(f, ...)}
#' is mapped to a unique task id, which is run on a separate node.
#'  \code{setup()} makes this easy by writing
#' the argument grid (\code{arg_grid.Rdata}), an R script to run one combination of arguments, a submission
#' script assigning all rows to a unique task id, seeds (if specified), and folders to store results in
#' a given directory. Jobs are submitted to the scheduler by running \code{qsub submit}
#' at the prompt, or by running \code{submit()} within R.
#'
#' The argument grid (\code{arg_grid}) is saved to \code{.dir} as \code{arg_grid.Rdata}.
#' It contains the columns of \code{expand.grid(...)} from \code{grid_apply(.f, ...)}.
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
#' be done by \code{Sys.getenv("SGE_TASK_ID")}. This might be used to
#' cache intermediate results.
#'
#'
#' If \code{.seed} is given, a list of seeds is generated in \code{seeds.Rdata} using
#'  L'ecuyer-CMRG streams for reproducible random number generation. A unique seed is
#'  generated for each independent job in the argument grid.
#'  Subsequent calls to setup using the same .seed generate the same seeds and reproducible results.
#'  See \code{parallel::nextRNGStream} for more details.
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
#' prompts the user for verification that an overwrite is intended, or stops with an error
#' if not run interactively.
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
#'  \link{test_job} Runs a job with a given id on the head node.
#'  \link{filter_jobs} writes a submission script for jobs matching conditions as in \code{dplyr::filter}
#'  \link{sge_env} can be used to access environmental variables.
setup <- function(object, ...){
  UseMethod("setup")
}

#' @export
#' @describeIn setup Setup sge files from \code{gapply, grid_apply}
setup.gapply <- function(object,
                  .dir=getwd(),
                  .reps=1,
                  .seed=NULL,
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

#' @export
setup.gresults <- setup.gapply

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
