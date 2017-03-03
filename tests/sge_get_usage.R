# get sge usage strings

sample_usages <- function(wait=.1, samples=50){
  x <- list()
  for(i in 1:samples){
    cat(i, " ")
    jids <- try(all_job_ids())
    x[[i]] <- try(qst_meta(jids,xml = TRUE))
    Sys.sleep(wait)
  }
  return(x)
}
sge_test(wait=10, queue="debug"); xl <- sample_usages()

is.err <- function(x){class(x) == "try-error"}
xl <- xl[!sapply(xl, is.err)]
dput(xl, file="qstat_xml.R")

get_qstat <- function(wait=.1, samples=50){
  x <- list()
  for(i in 1:samples){
    x[[i]] <- qst()
  }
}

