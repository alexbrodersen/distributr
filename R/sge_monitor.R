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
#' @export
qstat <- function(user=TRUE){

  if(user){
    jstr <- qst()
  } else {
    jstr <- system("qstat", intern=TRUE)
  }

  lines <- strsplit(jstr, "\n")
  # column names separated by whitespace
  # ----------------------
  # data separated by whitespace

  lines[[2]] <- NULL

  var_names <- strsplit(lines[[1]], "\\s+")[[1]]
  lines[[1]] <- NULL
  lp <- lapply(lines, function(l){ strsplit(l[[1]], "\\s+")[[1]]})
  df <- data.frame(do.call(rbind, lapply(lp, function(l){l[-1]})), stringsAsFactors = FALSE)
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
  return(df)
}

