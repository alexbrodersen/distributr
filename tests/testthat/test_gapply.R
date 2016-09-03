context("do.rep")

## write tests for different numbers of parameters!

do.one <- function(a=1,b=2){a+b}
r <- do.rep(do.one,list(a=1,b=2), .reps=2)
expect_equal(length(r), 2)
expect_is(r, "list")
expect_null(names(r))
r <- do.rep(do.one,list(a=1,b=2),.reps=8, .verbose=0, .rep.cores=1)
expect_equal(length(r), 8)
expect_null(names(r))

do.one <- function(a=1,b=2){c(a+b, a-b)}
r <- do.rep(do.one, list(a=1,b=2), .reps=2)
expect_null(names(r))

## Testing .args (must be a list, doesn't have to be named, must match the arguments.)
f <- function(a, b=1, x=1){ a + b + x}
x <- do.rep(f, list(b=0, x=0), .reps=2, .args=list(5))
expect_true(all(unlist(x) == 5))
x <- do.rep(f, list(a=0, x=0), .reps=2, .args=list(b=3))
expect_true(all(unlist(x) == 3))
x <- do.rep(f, list(a=0, x=0), .reps=2, .args=list(4)) ## will still match b
expect_true(all(unlist(x) == 4))
expect_error(do.rep(f, list(a=0, x=0, b=0), .reps=2, .args=list(b=3)))
x <- do.rep(f, list(a=0, x=0), .reps=2, .args=list(rnorm(5))) ## will still match b
expect_true(all(sapply(x, length) == 5))
f <- function(x){-x}
x <- do.rep(.f=f, .args=list(5), .reps=1)
expect_true(unlist(x) == -5)
x <- do.rep(.f=f, .args=list(5), .reps=1)

## Testing errors and warnings
context("wrapWE")

do.one <- function(a=1,b=2){
  if(a==1){ stop("asdf")}
  if(b==2){warning("this is a warning")}
  return(data.frame(perf=c(a+b, a-b)))
}

x <- do.rep(wrapWE(do.one), list(a=1, b=2), .reps=2)
expect_true(all(is.na(unlist(x))))
expect_true(all(sapply(x, function(x){ attr(x, "err")}) == "asdf"))
x <- do.rep(wrapWE(do.one), list(a=2, b=2), .reps=2)
expect_true(all(unlist(x) == c(4,0,4,0)))
expect_true(all(sapply(x, function(x){ attr(x, "warn")}) == "this is a warning"))

context("grid_apply")
out <- gapply(do.one, a=c(2,1), b=2, .reps=2, .verbose=0)
expect_true(nrow(out) == 6)

expect_true(!is.null(attr(out, "err")))
expect_true(!is.null(attr(out, "warn")))
expect_true(all(as.numeric(names(attr(out,"err"))) == c(3,4)))
expect_true(all(as.numeric(names(attr(out,"warn"))) == c(1,2)))
expect_true(all(is.na(out[out$a==1,"value"])))  # all errors return NA
expect_true(all(out[out$a==2,"value"] == c(4,0,4,0))) # warnings still return values

out <- gapply(do.one, a=c(2,1), b=2, .reps=2, .verbose=0)
out1 <- grid_apply(do.one, a=c(2,1), b=2, .reps=2, .verbose=0) %>% tidy.gresults()
expect_equivalent(out, out1)



## Single named return value
do.one <- function(a=1,b=2){c(a+b)}
out <- gapply(do.one, a=1:2, b=2, .reps=2, .verbose=0)
out1 <- grid_apply(do.one, a=1:2, b=2, .reps=2, .verbose=0) %>% tidy.gresults()

grid <- expand.grid(a=1:2,b=2)
expect_equal(colnames(out), c("a", "b", "rep", "key","value"))
expect_equal(nrow(out), nrow(grid)*2)
expect_equivalent(out$value,c(3,3,4,4))
expect_equivalent(unique(out[,c("a","b")]), grid)
expect_equivalent(out, out1)

## Multiple unnamed return values
## Names should be assigned by as.data.frame rules (V1, V2)
do.one <- function(a=1,b=2){c(a+b, a - b)}
out <- gapply(do.one,.reps=2, a=1:2,b=2,.verbose=0)
out1 <- grid_apply(do.one,.reps=2, a=1:2,b=2,.verbose=0) %>% tidy.gresults()
expect_equal(colnames(out),c("a","b","rep", "key", "value"))
expect_equivalent(unique(out[,c("a","b")]), grid)
expect_equal(unique(out$key),c("V1","V2"))
expect_equivalent(out, out1)
ans <- c(1+2, 1-2, 1+2, 1-2, 2 + 2, 2 - 2, 2 + 2, 2 - 2)
expect_equal(out$value, ans)

## Multiple named return values
do.one <- function(a=1,b=2){data.frame(sum=a+b,sub=a-b)}
out <- gapply(do.one,.reps=2, a=1:2,b=2,.verbose=0)
out1 <- grid_apply(do.one,.reps=2, a=1:2,b=2,.verbose=0) %>% tidy.gresults()
expect_equal(colnames(out),c("a","b","rep", "key", "key2", "value"))
# Test that key is a factor, and has the correct levels
expect_equal(unique(out$key),c("sum","sub"))
expect_equivalent(out, out1)
expect_equal(out$value, ans)

## One row in param.grid with multiple reps
do.one <- function(a=1,b=2){data.frame(sum=a+b,sub=a-b)}
out <- gapply(do.one,.reps=3, a=1,b=2,.verbose=0)
out1 <- grid_apply(do.one,.reps=3, a=1,b=2,.verbose=0) %>% tidy.gresults()
expect_equal(unique(out$rep),1:3)
expect_equal(out$value,c(3,-1, 3, -1, 3, -1))
expect_equivalent(out, out1)

## Data frame returns

do.one <- function(a=1,b=2){
  return(data.frame(plus=c(a+b, a+2*b, a+3*b), minus=c(a-b, a-2*b, a-3*b)))
}
out <- do.rep(do.one,list(a=1,b=2), .reps=3, .verbose=0, .eval=T)
expect_true(length(out) == 3)
expect_is(out[[1]], "data.frame")

out <- gapply(do.one, .reps=3, a=1:2, b=2, .verbose=0, .eval=T)
out1 <- grid_apply(do.one, .reps=3, a=1:2, b=2, .verbose=0, .eval=T) %>% tidy.gresults()
expect_true(all(unique(out$key) == c("plus", "minus")))
expect_true(all(unique(out$key2) == 1:3))

ans = c(rep(c(1 + 2, 1 + 2*2, 1 + 3*2, 1 - 2, 1 - 2*2, 1 - 3*2), times = 3),
        rep(c(2 + 2, 2 + 2*2, 2 + 3*2, 2 - 2, 2 - 2*2, 2 - 3*2), times = 3))

expect_equal(out$value, ans)
nm = 3 # number of key2s (a+b etc)
np = 2 # number of performance (minus, plus)
expect_true(nrow(out) == 3*nm*np*2)
expect_equivalent(out, out1)

# Different numbers of rows
do.one <- function(a){
  return(rep(a, a))
}
out <- gapply(do.one, a=1:5)
out1 <- grid_apply(do.one, .reps = 1, a=1:5, .verbose=0, .eval=T) %>% tidy.gresults()
expect_true(all(out$value == rep(1:5, 1:5)))
expect_equivalent(out, out1)


## .verbose
do.one <- function(a=1,b=2){
  return(data.frame(plus=c(a+b, a+2*b, a+3*b), minus=c(a-b, a-2*b, a-3*b)))
}
expect_output(out <- gapply(do.one,.reps=2, a=1:3,b=2:5,.verbose=1), ".")
expect_output(out <- gapply(do.one,.reps=2, a=1:3,b=2:5,.verbose=2), "a = ")
expect_output(out <- gapply(do.one,.reps=2, a=1:3,b=2:5,.verbose=3), "plus minus")
expect_output(out1 <- grid_apply(do.one,.reps=2, a=1:3,b=2:5,.verbose=1), ".")
expect_output(out1 <- grid_apply(do.one,.reps=2, a=1:3,b=2:5,.verbose=2), "a = ")
expect_output(out1 <- grid_apply(do.one,.reps=2, a=1:3,b=2:5,.verbose=3), "plus minus")


## elapsed time
do.one <- function(a=1,b=2){Sys.sleep(1); return(1)}
out <- gapply(do.one,.reps=2,.verbose=1, a=1,b=1)
out1 <- grid_apply(do.one,.reps=2,.verbose=1, a=1,b=1)
expect_is(attr(out,"time"), "proc_time")
expect_is(attr(out1, "time"), "proc_time")

# Test eval
do.one <- function(a=1,b=2){data.frame(sum=a+b,sub=a-b)}
out <- do.rep(do.one,list(a=1,b=2), .reps=3, .verbose=0, .eval=F)
expect_equal(unlist(out), rep(NA,3))

out <- gapply(do.one,.reps=3, a=1,b=2,.verbose=0, .eval=F)
expect_equal(unlist(out$value), rep(NA,3))

out <- grid_apply(do.one,.reps=3, a=1,b=2,.verbose=0, .eval=F) %>% tidy.gresults()
expect_equal(unlist(out$value), rep(NA,3))





