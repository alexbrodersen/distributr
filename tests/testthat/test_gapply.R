require(dplyr, quietly = T)
context("gapply")

test_that("do.rep", {
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
})

test_that("do.rep .args", {
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
})


do.one <- function(a=1,b=2){
  if(a==1){ stop("asdf")}
  if(b==2){warning("this is a warning")}
  return(data.frame(perf=c(a+b, a-b)))
}

## Testing errors and warnings
test_that("wrapWE", {
  x <- do.rep(wrapWE(do.one), list(a=1, b=2), .reps=2)
  expect_true(all(is.na(unlist(x))))
  expect_true(all(sapply(x, function(x){ attr(x, "err")}) == "asdf"))
  x <- do.rep(wrapWE(do.one), list(a=2, b=2), .reps=2)
  expect_true(all(unlist(x) == c(4,0,4,0)))
  expect_true(all(sapply(x, function(x){ attr(x, "warn")}) == "this is a warning"))
})


test_that("grid_apply", {
  out <- gapply(do.one, a=c(2,1), b=2, .reps=2, .verbose=0, .stack=T)
  expect_true(nrow(out) == 6)

  expect_true(!is.null(attr(out, "err")))
  expect_true(!is.null(attr(out, "warn")))
  expect_true(all(as.numeric(names(attr(out,"err"))) == c(3,4)))
  expect_true(all(as.numeric(names(attr(out,"warn"))) == c(1,2)))
  expect_true(all(is.na(out[out$a==1,"value"])))  # all errors return NA
  expect_true(all(out[out$a==2,"value"] == c(4,0,4,0))) # warnings still return values

  out <- gapply(do.one, a=c(2,1), b=2, .reps=2, .verbose=0, .stack=T)
  out1 <- grid_apply(do.one, a=c(2,1), b=2, .reps=2, .verbose=0) %>% tidy(., stack=T)
  expect_equivalent(out, out1)
})

test_that("grid_apply single named return value", {
  do.one <- function(a=1,b=2){c(a+b)}
  out <- gapply(do.one, a=1:2, b=2, .reps=2, .verbose=0)
  out1 <- grid_apply(do.one, a=1:2, b=2, .reps=2, .verbose=0) %>% tidy

  grid <- expand.grid(a=1:2,b=2)
  expect_equal(colnames(out), c("a", "b", ".rep","value"))
  expect_equal(nrow(out), nrow(grid)*2)
  expect_equivalent(out$value,c(3,3,4,4))
  expect_equivalent(unique(out[,c("a","b")]), grid)
  expect_equivalent(out, out1)
})

test_that("grid_apply multiple unnamd return values", {
  do.one <- function(a=1,b=2){c(a+b, a - b)}
  grid <- expand.grid(a=1:2,b=2)

  out <- gapply(do.one,.reps=2, a=1:2,b=2,.verbose=0)
  out1 <- grid_apply(do.one,.reps=2, a=1:2,b=2,.verbose=0) %>% tidy
  expect_equivalent(out, out1)
  ans1 <- c(1+2, 1+2, 2+2, 2+2)
  ans2 <- c(1-2, 1-2, 2-2, 2-2)
  expect_equal(out[,4], ans1)
  expect_equal(out[,5], ans2)

  out <- gapply(do.one,.reps=2, a=1:2,b=2,.verbose=0, .stack = T)
  out1 <- grid_apply(do.one,.reps=2, a=1:2,b=2,.verbose=0) %>% tidy(., stack=T)
  expect_equal(colnames(out),c("a","b",".rep", "key", "value"))
  expect_equivalent(unique(out[,c("a","b")]), grid)
  expect_equal(unique(out$key),c("V1","V2"))
  expect_equivalent(out, out1)
  ans <- c(1+2, 1-2, 1+2, 1-2, 2 + 2, 2 - 2, 2 + 2, 2 - 2)
  expect_equal(out$value, ans)
})

test_that("grid_apply Multiple named return values", {
  do.one <- function(a=1,b=2){data.frame(sum=a+b,sub=a-b)}
  out <- gapply(do.one,.reps=2, a=1:2,b=2,.verbose=0, .stack=F)
  out1 <- grid_apply(do.one,.reps=2, a=1:2,b=2,.verbose=0) %>% tidy(., stack=F)
  ans1 <- c(1+2, 1+2, 2+2, 2+2)
  ans2 <- c(1-2, 1-2, 2-2, 2-2)
  expect_equal(out[,4], ans1)
  expect_equal(out[,5], ans2)
  expect_equivalent(out1, out)
  expect_equal(colnames(out1), c("a", "b", ".rep", "sum", "sub"))


  out <- gapply(do.one,.reps=2, a=1:2,b=2,.verbose=0, .stack=T)
  out1 <- grid_apply(do.one,.reps=2, a=1:2,b=2,.verbose=0) %>% tidy(., stack=T)
  expect_equal(colnames(out),c("a","b",".rep", "key", "key2", "value"))
  # Test that key is a factor, and has the correct levels
  expect_equal(unique(out$key),c("sum","sub"))
  expect_equivalent(out, out1)
  ans <- c(1+2, 1-2, 1+2, 1-2, 2 + 2, 2 - 2, 2 + 2, 2 - 2)
  expect_equal(out$value, ans)

})

test_that("grid_apply .reps", {
  do.one <- function(a=1,b=2){data.frame(sum=a+b,sub=a-b)}
  out <- gapply(do.one,.reps=3, a=1,b=2,.verbose=0)
  out1 <- grid_apply(do.one,.reps=3, a=1,b=2,.verbose=0) %>% tidy
  expect_equivalent(out, out1)
  expect_equal(unique(out$.rep),1:3)
  expect_equal(out$sum, c(3,3,3))
  expect_equal(out$sub, c(-1,-1,-1))

  out <- gapply(do.one,.reps=3, a=1,b=2,.verbose=0, .stack=T)
  expect_equal(out$value, c(3,-1, 3, -1, 3, -1))
})


test_that("grid_apply data frame returns unequal returns", {
  # Different numbers of rows
  do.one <- function(a){
    return(rep(a, a))
  }
  out <- gapply(do.one, a=1:5, .stack=T)
  out1 <- grid_apply(do.one, .reps = 1, a=1:5, .verbose=0, .eval=T) %>% tidy(., stack=T)
  expect_true(all(out$value == rep(1:5, 1:5)))
  expect_equivalent(out, out1)

  # output returns named and un-unamed elements
  do.one <- function(a = 1, b = 2) {
    if (a == 1)
      stop("asdf")
    a
  }
  out <- gapply(do.one, a=1:5)
  out1 <- grid_apply(do.one, .reps = 1, a=1:5, .verbose=0, .eval=T) %>% tidy
  expect_equal(out$value, c(NA, 2:5))
  out2 <- gapply(do.one, a=1:5, .stack=T)
  expect_equal(out2$value, c(NA, 2:5))
})


test_that("grid_apply list returns", {
  do.one <- function(a=1, b=2){
    return(list(x=a + b, y=a - b))
  }
  out <- gapply(do.one, a=1:2, b=1:2, .reps = 1)
  out1 <- grid_apply(do.one, a=1:2, b=1:2, .reps = 1) %>% tidy
  expect_equivalent(out, out1)
  expect_equal(unlist(out1$x), c(1+1, 2+1, 1+2, 2+2))
  expect_equal(unlist(out1$y), c(1-1, 2-1, 1-2, 2-2))

  out <- gapply(do.one, a=1:2, b=1:2, .reps = 1, .stack=T)
  expect_equal(out$key, rep(c("x", "y"), times = 4))
  expect_equal(out$value, c(2, 0, 3, 1, 3, -1, 4, 0))
})

test_that("grid_apply list of vectors", {
  do.one <- function(a=1, b=2){
    return(list(x=c(a, a), y=c(b, b)))
  }
  out <- gapply(do.one, a=1:2, b=1:2, .reps = 1)
  out1 <- grid_apply(do.one, a=1:2, b=1:2, .reps = 1) %>% tidy
  expect_equivalent(out, out1)
  expect_equal(out$x, list(c(1,1), c(2,2), c(1,1), c(2,2)))
  expect_equal(out$y, list(c(1,1), c(1,1), c(2,2), c(2,2)))


  out <- gapply(do.one, a=1:2, b=1:2, .reps = 1, .stack=T)
  expect_equal(out$key, rep(c("x", "x", "y", "y"), times = 4))
  expect_equal(out$key2, rep(c("1", "2"), times = 4*2))
  expect_equal(out$value, c(1, 1, 1, 1,
                            2, 2, 1, 1,
                            1, 1, 2, 2,
                            2, 2, 2, 2))

  do.one <- function(a=1){
    return(rnorm(a))
  }
  expect_error(out <- gapply(do.one, a=1:5))
  out <- grid_apply(do.one, a=1:5)
  ans <- unlist(out)
  expect_equal(ans, tidy(out, stack=T)$value)
})

test_that("grid_apply list of vectors of unequal length", {
  do.one <- function(a=1, b=2){
    return(list(x=rep(a,a), y=rep(b,b)))
  }
  out <- gapply(do.one, a=1:2, b=1:2, .reps = 1)
  expect_equal(out$x, list(c(1), c(2,2), c(1), c(2,2)))
  expect_equal(out$y, list(c(1), c(1), c(2,2), c(2,2)))

  out <- gapply(do.one, a=1:2, b=1:2, .reps = 1, .stack=T)
  l <- grid_apply(do.one, a=1:2, b=1:2, .reps=1)
  expect_equivalent(out$value, unlist(l))
})

test_that("grid_apply .verbose", {
  do.one <- function(a=1,b=2){
    return(data.frame(plus=c(a+b, a+2*b, a+3*b), minus=c(a-b, a-2*b, a-3*b)))
  }
  expect_output(out <- gapply(do.one,.reps=2, a=1:3,b=2:5,.verbose=1), ".")
  expect_output(out <- gapply(do.one,.reps=2, a=1:3,b=2:5,.verbose=2), "a = ")
  expect_output(out <- gapply(do.one,.reps=2, a=1:3,b=2:5,.verbose=3), "plus minus")
  expect_output(out1 <- grid_apply(do.one,.reps=2, a=1:3,b=2:5,.verbose=1), ".")
  expect_output(out1 <- grid_apply(do.one,.reps=2, a=1:3,b=2:5,.verbose=2), "a = ")
  expect_output(out1 <- grid_apply(do.one,.reps=2, a=1:3,b=2:5,.verbose=3), "plus minus")
})

test_that("elapsed time", {
  do.one <- function(a=1,b=2){Sys.sleep(.1); return(1)}
  out <- gapply(do.one,.reps=2,.verbose=1, a=1,b=1)
  out1 <- grid_apply(do.one,.reps=2,.verbose=1, a=1,b=1)
  expect_is(attr(out,"time"), "proc_time")
  expect_is(attr(out1, "time"), "proc_time")
})

test_that("grid_apply .eval", {
  do.one <- function(a=1,b=2){data.frame(sum=a+b,sub=a-b)}
  out <- do.rep(do.one,list(a=1,b=2), .reps=3, .verbose=0, .eval=F)
  expect_equal(unlist(out), rep(NA,3))

  out <- gapply(do.one,.reps=3, a=1,b=2,.verbose=0, .eval=F)
  expect_equal(unlist(out$value), rep(NA,3))

  out <- grid_apply(do.one,.reps=3, a=1,b=2,.verbose=0, .eval=F) %>% tidy
  expect_equal(unlist(out$value), rep(NA,3))
})

test_that("grid_apply .args", {
  do.one <- function(a=1, b=2, dat=NULL){ dat[a,b]}
  d <- data.frame(rnorm(5), rnorm(5))
  outl <- grid_apply(do.one, a=1:3, b=1:2, .args=list(dat=d))
  out <- gapply(do.one, a=1:3, b=1:2, .args=list(dat=d))
  ans <- unname(unlist(d[1:3, ]))
  expect_equal(unlist(outl), ans)
  expect_equal(out$value, ans)
})

test_that("tidy nested", {
  do.inner <- function(x, y){x+y}
  do.one <- function(a, b){
    gapply(do.inner, x=c(a, a+b), y=c(b, b+a), .reps=2)
  }
  res <- grid_apply(do.one, a=1:2, b=1:2, .reps=1) %>% tidy
  gr <- expand.grid(a=1:2, b=1:2)
  ans <- lapply(split(gr, 1:nrow(gr)), function(arg){
    gapply(do.inner, x=c(arg$a, arg$a+arg$b), y=c(arg$b, arg$b+arg$a), .reps=2)}) %>% bind_rows
  expect_equal(ans$value, res$value)
})

