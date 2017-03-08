#' Return running jobs as a string
#' @export
qst <- function(){
  system("qstat -u $USER", intern=TRUE)
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

#' Parse qst(xml=T) to data frame
#' @param jstr xml string from \code{qst()}
#' @importFrom xml2 read_xml xml_find_all xml_children xml_text xml_name
#' @export
parse_qstat <- function(jstr){
  info <- read_xml(paste0(jstr, collapse=""))
  job_list <- xml_find_all(info, ".//job_list")

  node_names <- unique(xml_name(xml_children(job_list)))
  get_all_node_text <- function(nn){ xml_text(xml_find_all(job_list, nn))}

  text_cols <- lapply(node_names, get_all_node_text)
  names(text_cols) <- node_names
  info_df <- data.frame(text_cols, stringsAsFactors = F)

  # do additional parsing if not empty
  if(nrow(info_df) > 0){
    # convert tasks in qw state to min(task)
    min_task <- function(x){
      min(as.numeric(regmatches(x, gregexpr("\\d*", x))[[1]]), na.rm=T)
    }
    info_df[info_df$state == "qw", "tasks"] <-
      sapply(info_df[info_df$state == "qw", "tasks"], min_task)

    new_names <- c("jid","prior","name","user","state", "start","queue","jclass",
                   "slots", ".sge_id")
    colnames(info_df) <- new_names

    # Get types of cols correct
    numeric_cols <- c("jid", "prior", "slots", ".sge_id")
    info_df[numeric_cols] <- lapply(info_df[numeric_cols], as.numeric)

    # Parse time from string
    date_str <- gsub("T", " ", info_df$start)
    info_df$start <- as.POSIXct(strptime(date_str, format("%Y-%m-%d %H:%M:%S")))
  }

  return(info_df)
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

