#' \code{summary} method for class \code{"gapply"}
#' @param object gapply result object
#' @param .reps number of reps to scale to
#' @param .fun function to aggregate over replications
#' @param .key A string of the character vector to filter on
#' @return Prints the means over all reps for each condition and returns it invisibly. Also prints the estimated time to scale up to x reps
#' @importFrom dplyr group_by_ summarize
#' @importFrom tidyr %>%
#' @export
summary.gapply <- function(object, .reps=NULL, .fun=mean, .key=NULL){
  ns <- c(attr(object, 'arg.names'),"key")
  if(is.null(.key)){
    .key <- as.character(unique(object$key))
  }
  
  res <- object %>% 
    dplyr::group_by_(.dots=ns) %>% 
    dplyr::filter(key %in% .key) %>%
    dplyr::summarize(.fun(value)) # I don't like it, but it will work for now

  print(res)
  cat("",fill=T)
  grid <- attr(object, "param.grid")
  cat("Number of conditions: ", nrow(grid), fill=T)
  print(head(grid))
  cat("",fill=T)
  if(!is.null(attr(object,"time"))){
    cat("Estimated time for x reps:", fill=T)
    cat("Reps \t Time", fill=T)
    o <- estimate.time(object, nreps=.reps)
    for(i in 1:nrow(o)){ cat(paste0(o[i,],"\t"),fill=T)}
  }
  invisible(res)
}

#' Plot simulation object
#' 
#' Automatic plotting method for aggregating over replications
#' @export
plot.gapply <- function(object, .fun=mean, .key=NULL){
  require(ggplot2)
  sum <- summary(object, .fun=.fun, .key=.key)
  arg.names <- attr(object, "arg.names")
  levels <- lapply(object[arg.names], unique)
  num.levels <- unlist(lapply(levels, length))
  map.args <- arg.names[order(num.levels,decreasing=T)] # arg with the most levels is first
  n.args <- length(map.args)
  key.levels <- length(unique(sum$key))
  
  
  if(key.levels > 1){
    if(n.args == 1){
      ## Simple box plot of all the reps
      g <- ggplot(object, aes(y="value", x=map.args[1])) +
        geom_boxplot()
      return(g)
    }
    if(n.args == 2){
      ## map second varying factor onto color, and reduce reps by .fun
      g <- ggplot(sum, aes(y=".fun(value)", x=map.args[1], col=map.args[2], group=map.args[2])) +
        geom_line() + geom_point()
      return(g)
    }
    if(n.args == 3){
      form <- as.formula(paste0(". ~",map.args[3]))
    }
    if(n.args == 4){
      form <- as.formula(paste0(map.args[4]," ~ ",map.args[3]))
    }
    if(n.args >= 5){
      rhs <- paste0(map.args[5:n.args], collapse = " + ")
      form <- as.formula(paste0(map.args[4]," ~ ", rhs))
    }
    g <- ggplot(sum, aes(y=".fun(value)", x=map.args[1], col=map.args[2], group=map.args[2])) +
      geom_line() + geom_point() +
      facet_grid(form)
    return(g)
  } else { # multiple keys
    lhs <- paste0("key", collapse = " + ")
    rhs <- paste0(map.args[3:n.args], collapse = " + ")
    rhs <- gsub("NA \\+ ", "", rhs)
    form <- paste0(lhs, " ~ ", rhs)
    if(n.args == 1){
      ## Simple box plot of all the reps
      g <- ggplot(object, aes(y="value", x=map.args[1])) +
        geom_boxplot()
      return(g)
    }
    paste0("ggplot(sum, aes(y='.fun(value)', x=", map.args[1], ", col=", map.args[2], ", group=", map.args[2])
    g <- ggplot(sum, aes(y=".fun(value)", x=as.symbol(map.args[1]), col=map.args[2], group=map.args[2])) +
          geom_line() + geom_point() +
          facet_grid(form)
  }
  return(g)
}

#' Estimate time for a given number of reps
#' @param object gapply object
#' @param nreps number of reps to scale to
#' @export
estimate.time <- function(object, nreps=NULL){
  if(is.null(nreps)){ nreps <- c(50,100,500,1000,5000,10000)}
  max.reps <- max(object$rep)
  time.per.rep <- attr(object,"time")[3]/max.reps
  times <- lapply(time.per.rep*nreps,FUN=nicetime)
  o <- cbind(reps=nreps,times=times)
  return(o)  
}

## From package astro
nicetime <- function (seconds) {
  lapseconds = round(seconds)
  seconds = lapseconds%%60
  minutes = ((lapseconds - seconds)/60)%%60
  hours = ((lapseconds - minutes * 60 - seconds)/3600)%%24
  days = ((lapseconds - hours * 3600 - minutes * 60 - seconds)/86400)
  lapline = {
  }
  if (days != 0) {
    if (days == 1) {
      lapline = paste(days, "d, ", sep = "")
    }
    else {
      lapline = paste(days, "d, ", sep = "")
    }
  }
  if (hours != 0 | days != 0) {
    if (hours == 1) {
      lapline = paste(lapline, hours, "h, ", sep = "")
    }
    else {
      lapline = paste(lapline, hours, "h, ", sep = "")
    }
  }
  if (minutes != 0 | hours != 0 | days != 0) {
    if (minutes == 1) {
      lapline = paste(lapline, minutes, "m, ", sep = "")
    }
    else {
      lapline = paste(lapline, minutes, "m, ", sep = "")
    }
  }
  if (seconds == 1) {
    lapline = paste(lapline, seconds, "s", sep = "")
  }
  else {
    lapline = paste(lapline, seconds, "s", sep = "")
  }
  return(lapline)
}