#' Return running jobs as a string
#' @export
qst <- function(xml=FALSE){
  cmd <- "qstat -u $USER"
  if(xml) cmd <- paste0(cmd, " -xml")
  system(cmd, intern=TRUE)
}

#' Retrieve meta data for a job id (in xml)
#' @param jid job id
#' @param xml whether xml is returned (default: \code{FALSE})
#' @export
qst_meta <- function(jid, xml=FALSE){
  cmd <- paste0("qstat -j ", jid)
  if(xml) cmd <- paste(cmd, " -xml")
  system(cmd, intern=T)
}


#' Retrieve jobids of currently running jobs
#' @export
all_job_ids <- function(){
  jstr <- qst()
  df <- distributr:::parse_qstat(jstr)
  jids <- as.numeric(unique(df[,"job_id"]))
  return(jids)
}

#' Meta-data for currently running jobs
#'
#' The meta-data of running SGE jobs are returned as a data frame. The
#' meta includes status, resources requested, memory usage, and wallclock/cpu time.
#' Works for all grid engine jobs and task arrays, including the ones created by \code{distributr}.
#' @param user job meta-data is returned for user jobs only (default: \code{TRUE}),
#' otherwise meta-data for all jobs in default queue is returned
#' @return A data frame with the following columns:
#' \item{job_id}{SGE job id}
#' \item{.sge_id}{the value of \code{SGE_TASK_ID} for the job}
#' \item{prior}{Priority of the job}
#' \item{name}{job name}
#' \item{user}{user name}
#' \item{state}{ state of the job, e.g. \code{r} for running}
#' \item{start}{ date and time of job start}
#' \item{queue}{queue and machine job is running on}
#' \item{jclass}{class of job (usually \code{NA})}
#' \item{slots}{Number of slots (cores) job is running with}
#' \item{maxvmem}{Maximum virtual memory used for the job (GB)}
#' \item{mem}{Current physical memory used for the job (GB)}
#' \item{vmem}{Current virtual memory used for the job (GB)}
#' \item{wallclock}{Amount of time the job has been running (sec)}
#' \item{cpu}{Amount of CPU time the job has used (sec)}
#' The columns \code{prior}, \code{user}, \code{start}, \code{queue}, and \code{jclass}
#' are not printed. If no jobs are running or in the queue, returns \code{data.frame()}.
#' For queued jobs, \code{queue = NA}, and \code{.sge_id} is the lowest task
#' still in the queue. Does not work for jobs with job classes.
#' @export
#' @examples
#' \dontrun{
#'
#'sge_test(wait=20)
#'qstat()
#' }
qstat <- function(user=TRUE){

  if(user){
    jstr <- qst()
  } else {
    jstr <- system("qstat", intern=TRUE)
  }
  if(length(jstr) > 0){
    df <- parse_qstat(jstr)
    jids <- unique(df[,"job_id"])
    job_usage <- lapply(jids, function(jid){
      system(paste0("qstat -j ", jid), intern=T)})
    if(length(job_usage) > 0){
      usage_df <- do.call(rbind, lapply(job_usage, parse_usage))
      df <- merge(df, usage_df, by=c("job_id", ".sge_id"))
      df$status <- NULL
    } else {
      df$wallclock <- NA
      df$cpu <- NA
      df$mem <- NA
      df$vmem <- NA
      df$maxvmem <- NA
    }
  } else {
    # can't parse
    df <- jstr
  }
  class(df) <- c("qstat", "data.frame")
  return(df)
}

#' @export
print.qstat <- function(x, ...){
  obj <- x
  if(nrow(x) > 0){
    x$prior <- NULL
    x$user <- NULL
    x$start <- NULL
    x$queue <- NULL
    x$jclass <- NULL
    if(!any(is.na(x$wallclock))) x$wallclock <- sapply(x$wallclock, nicetime)
    if(!any(is.na(x$cpu))) x$cpu <- sapply(x$cpu, nicetime)
    if(!any(is.na(x$mem))) x$mem <- formatC(x$mem, digits=2)
    if(!any(is.na(x$vmem))) x$vmem <- formatC(x$vmem, digits=2)
    if(!any(is.na(x$maxvmem))) x$maxvmem <- formatC(x$vmem, digits=2)
  }
  print.data.frame(x)
  comment <- paste0("... with 5 more variables: prior, user, start, queue, jclass")
  cat(comment, sep = "\n")

  invisible(obj)
}

parse_qstat <- function(jstr){
  lines <- strsplit(jstr, "\n")
  # Sample job string:
  #"job-ID     prior   name       user         state submit/start at     queue                          jclass   slots ja-task-ID ",
  #"------------------------------------------------------------------------------------------------------------------------------------------------",
  #"    740473 0.60142 sleep_ss_l pmille13     r     02/08/2017 15:38:00 long@q16copt036.crc.nd.edu               24 1",
  #"    740473 0.60142 sleep_ss_l pmille13     r     02/08/2017 15:38:00 long@q16copt036.crc.nd.edu               24 2",
  #"    743563 0.00000 distributr pmille13     qw    02/11/2017 14:10:31

  lines[[2]] <- NULL
  var_names <- strsplit(lines[[1]], "\\s+")[[1]]
  lines[[1]] <- NULL
  job_list <- lapply(lines, function(l){ strsplit(l[[1]], "\\s+")[[1]]})
  job_stand <- lapply(job_list, standardize_cols, names=var_names)
  df <- data.frame(do.call(rbind, job_stand))

  # types of columns
  is_numeric <- which(colnames(df) %in% c("job-ID", "prior", "slots", "ja.task.ID"))
  df[, is_numeric] <- lapply(df[, is_numeric], as.numeric)
  date_str <- paste0(df[,"submit.start"], " ", df[,"at"])
  df$start <- as.POSIXct(strptime(date_str, format("%m/%d/%Y %H:%M:%S")))
  df[,"at"] <- NULL
  df[,"submit.start"] <- NULL

  # reorder
  df <- df[,c(1:5, 10, 6:9)]
  colnames(df)[c(1, 10)] <- c("job_id", ".sge_id")
  return(df)
}

# confine all the hacks to here!
#var_names
#[1,] "X1"  "job-ID"
#[2,] "X2"  "prior"
#[3,] "X3"  "name"
#[4,] "X4"  "user"
#[5,] "X5"  "state"
#[6,] "X6"  "submit/start"
#[7,] "X7"  "at"
#[8,] "X8"  "queue"
#[9,] "X9"  "jclass"
#[10,] "X10" "slots"
#[11,] "X11" "ja-task-ID"
# Todo: add suport for job classes...
standardize_cols <- function(x, names){
  x <- data.frame(t(x[-1]), stringsAsFactors = F)
  df <- NULL
  if(any(grepl("qw", x))){
    if(ncol(x) == 8){
      # not a task array (use 1 as default)
      df <- data.frame(cbind(x[1:7], X8=NA, X9=NA, X10=x[8], X11=1), stringsAsFactors = F)
    } else {
      # is a task array (use lower for qw)
      task_str <- x[[9]]
      arange <- regmatches(task_str, regexpr("(\\d*-\\d*:\\d)", task_str))
      task_range <- as.numeric(regmatches(arange, gregexpr("\\d*", arange))[[1]])
      task_low <- task_range[1]
      #task_up <- task_range[3]
      df <- data.frame(cbind(x[1:7], X8=NA, X9=NA, X10=x[8], X11=task_low), stringsAsFactors = F)
    }
  } else {
    if(ncol(x) == 10){
      # no jclass, assign NA
      df <- data.frame(cbind(x[1:8], X9=NA, x[9:10]), stringsAsFactors = F)
    } else if(ncol(x) == 9){
      # not a task array
      # default task id is 1
      df <- data.frame(cbind(x[1:8], X9=NA, x[9], X11=1), stringsAsFactors = F)
    }
  }
  colnames(df) <- names
  return(df)
}

parse_usage <- function(mstr){
  job_id <- as.numeric(strsplit(grep("job_number", mstr, value=T), "\\s+")[[1]][2])

  job_state <- lapply(grep("job_state", mstr, value=T),
         function(x){ strsplit(x, "(\\s+)")[[1]]})
  status <- do.call(rbind, lapply(job_state, function(j){
    data.frame(.sge_id=as.numeric(gsub(":", "", j[2])),
               status = j[3], stringsAsFactors = F)}))
  # remove jobs that have exited
  status <- status[status$status != "x", ]
  if(is.null(status)){
    # grab the sge_task id
    task_str <- grep("job-array tasks", mstr, value=TRUE)
    arange <- regmatches(task_str, regexpr("(\\d*-\\d*:\\d)", task_str))
    task_range <- as.numeric(regmatches(arange, gregexpr("\\d*", arange))[[1]])
    task_low <- task_range[1]
    status <- data.frame(status=NA, .sge_id=task_low)
  }

  usage <- lapply(grep("usage", mstr, value=T),
                  function(l){ strsplit(l[[1]], "\\s+,?")[[1]]})

  get_info <- function(regex, str_list){
    sapply(str_list, function(l){grep(regex, l, value = T)})
  }

  maxvmem <- do.call(rbind, lapply(get_info("maxvmem", usage), mem_to_df))
  vmem <- do.call(rbind, lapply(get_info("^vmem", usage), mem_to_df))

  vmem$mem[vmem$unit %in% "M"] <- vmem$mem[vmem$unit %in% "M"]/1000
  maxvmem$mem[maxvmem$unit %in% "M"] <- maxvmem$mem[maxvmem$unit %in% "M"]/1000

  if(!any(is.na(status$status))){
    wallclock <- gsub(",", "", gsub("wallclock=", "",
                                    strsplit(get_info("wallclock", usage), "\\s+")))
    cpu <- gsub(",", "", gsub("cpu=", "",
                              strsplit(get_info("cpu", usage), "\\s+")))
    mem <- as.numeric(sapply(strsplit(get_info("^mem", usage), "="), `[`, 2))
    wall_sec <- clock_to_sec(wallclock)
    cpu_sec <- clock_to_sec(cpu)
    meta <- data.frame(job_id,
                       status,
                       maxvmem=maxvmem$mem,
                       mem=mem,
                       vmem=vmem$mem,
                       wallclock=wall_sec,
                       cpu=cpu_sec)
  } else {
    meta = data.frame(job_id, status, maxvmem=NA,
                      mem=NA, vmem=NA, wallclock=NA, cpu=NA)
  }
  return(meta)
}

clock_to_sec <- function(x){
  time <- lapply(strsplit(x, ":"), as.numeric)
  pad_days <- lapply(time, function(t){c(rep(0, 4-length(t)), t)})
  seconds <- sapply(pad_days, function(t){
    60*60*24*t[1] +
      60*60*t[2] +
      60*t[3] +
      t[4]})
  return(seconds)
}

mem_to_df <- function(x){
  if(grepl("N/A", x)){
    df <- data.frame(mem=NA, unit=NA)
  } else {
    match <- regmatches(x, regexec("(\\d*.\\d*)([A-Z])", x))[[1]]
    df <- data.frame(mem=as.numeric(match[2]), unit=match[3], stringsAsFactors = F)
  }
  return(df)
}

