#' Return running jobs as a string
#' @export
qst <- function(){
  system("qstat -u $USER", intern=TRUE)
}

#' Meta-data for currently running jobs
#' @param user job meta-data is returned for user jobs only (default: \code{TRUE}),
#' otherwise meta-data for all jobs in default queue is returned
#' @return A data frame with the following columns:
#' \item{job-ID}{SGE job id}
#' \item{prior}{Priority of the job}
#' \item{name}{job name}
#' \item{user}{user name}
#' \item{state}{ state of the job, e.g. \code{r} for running}
#' \item{start}{ date and time of job start}
#' \item{queue}{queue and machine job is running on}
#' \item{jclass}{class of job (usually \code{NA})}
#' \item{slots}{Number of slots (cores) job is running with}
#' \item{.sge_id}{the value of \code{SGE_TASK_ID} for the job}
#' If no jobs are running or in the queue, returns \code{character(0)} (empty string).
#' @export
qstat <- function(user=TRUE){

  if(user){
    jstr <- qst()
  } else {
    jstr <- system("qstat", intern=TRUE)
  }
  if(length(jstr) > 0){
    df <- parse_qstat(jstr)$run
  } else {
    df <- jstr
  }
  return(df)
}

parse_qstat <- function(jstr){
  lines <- strsplit(jstr, "\n")
  # Sample job string:
  #"job-ID     prior   name       user         state submit/start at     queue                          jclass                         slots ja-task-ID ",
  #"------------------------------------------------------------------------------------------------------------------------------------------------",
  #"    740473 0.60142 sleep_ss_l pmille13     r     02/08/2017 15:38:00 long@q16copt036.crc.nd.edu                                       24 1",
  #"    740473 0.60142 sleep_ss_l pmille13     r     02/08/2017 15:38:00 long@q16copt036.crc.nd.edu                                       24 2",
  #"    743563 0.00000 distributr pmille13     qw    02/11/2017 14:10:31                                                                   1 12105-24000:1"

  lines[[2]] <- NULL
  var_names <- strsplit(lines[[1]], "\\s+")[[1]]
  lines[[1]] <- NULL
  job_list <- lapply(lines, function(l){ strsplit(l[[1]], "\\s+")[[1]]})

  qw <- sapply(job_list, function(l){any(l == "qw")})
  running <- job_list[!qw]
  queued <- job_list[qw]

  df <- data.frame(do.call(rbind, lapply(running, function(l){l[-1]})), stringsAsFactors = FALSE)
  # ncol(df) == 10 if jclass is empty
  if(ncol(df) == 10){
    df <- cbind(df[,1:8], NA, df[,9:10])
  }
  colnames(df) <- var_names
  is_numeric <- which(colnames(df) %in% c("job-ID", "prior", "slots", "ja-task-ID"))
  df[, is_numeric] <- lapply(df[, is_numeric], as.numeric)
  date_str <- paste0(df[,"submit/start"], " ", df[,"at"])
  df$start <- strptime(date_str, format("%m/%d/%Y %H:%M:%S"))
  df[,"at"] <- NULL
  df[,"submit/start"] <- NULL
  df <- df[,c(1:5, 10, 6:9)]
  colnames(df)[10] <- ".sge_id"

  return(list(run=df, qw=queued))
}

