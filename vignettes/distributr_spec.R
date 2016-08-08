## ----setup, include=FALSE------------------------------------------------
library(xtable)
library(diagram)
library(distributr)
library(dplyr)

knitr::opts_chunk$set(echo = TRUE, eval=F)

## ---- eval=TRUE----------------------------------------------------------
#devtools::install_github("patr1ckm/distributr")
library(distributr) 
do.one <- function(n, mu, sd){ mean(rnorm(n, mu, sd)) }

sim <- gapply(do.one, n = c(50, 100, 500), mu = c(1,5), sd = c(1, 5, 10), 
              .reps=50, .mc.cores=5)
head(sim)

## ---- eval=TRUE----------------------------------------------------------
do.one <- function(a=1,b=2){
  if(a==1){ stop("asdf")}
  if(b==2){warning("this is a warning")}
  return(data.frame(ops1=c(a+b, a-b), ops2=c(a*b, a^2)))
}

sim <- gapply(do.one, a=c(2,1), b=2, .reps=2, .verbose=0)
sim

## ---- eval=TRUE----------------------------------------------------------
err(sim)
warn(sim)

## ------------------------------------------------------------------------
#  sim <- gapply(do.one, n = c(50, 100, 500), mu = c(1,5), sd = c(1, 5, 10), .eval=F)
#  setup(sim, .reps=500, .chunks = 3, .mc.cores = 5)
#  submit(sim)
#  res <- collect(sim)

## ---- eval=F-------------------------------------------------------------
#  workflow <- layer(node(f, arg1=c(1,2), arg2=c(T, F))) +
#    layer(node(g, arg3=c(.05, .001), arg4=c(10, 100)) +
#          node(g2, arg5=c(1,2)) +
#    layer(node(h), node(h2)) +
#    options(reps = 1000, tidy = TRUE, seed = 5) +  # workflow options
#    sge(queue = "long", dir = "test", shell = "bash", ...)  # options to sge environment

## ---- echo=F, eval=T, fig.width=5, fig.height=4--------------------------
 par(mar = c(1, 1, 1, 1), mfrow = c(1, 1), oma=c(1,1,1,1))
 names <- c("f()", "g()", "g2()", "h()")
M <- matrix(nrow = 4, ncol = 4, byrow = TRUE, data = 0)
M[2, 1] <- M[3, 1] <- M[4, 2] <- M[4, 3] <- ""
plotmat(M, pos = c(1, 2, 1), name = names, lwd= 1, relsize=1,
         box.lwd = 1, cex.txt=.8, box.type = "circle", box.prop=1.0)

#par(mar = c(1, 1, 1, 2), oma=c(1,1,1,3))
#names <- c("f()", "g()", "g2()", "h()", "h2()", "h()", "h2()")
#M <- matrix(nrow = 7, ncol = 7, byrow = TRUE, data = 0)
#M[2, 1] <- M[3, 1] <- M[4, 2] <- M[6, 3] <- M[5, 2] <- M[7, 3] <- ""
#plotmat(M, pos = c(1, 2, 4), name = names, lwd= 1, relsize=1,
#        box.lwd = 1, cex.txt=.8, box.type = "circle", box.prop=1.0)
xpos <- .95
text(xpos,.85, labels = "Level 1", las=.5, xpd=NA)
lines(x = c(xpos, xpos), y = c(.8, .55), xpd=NA)
text(xpos,.5, labels = "Level 2", las=.5, xpd=NA)
lines(x = c(xpos, xpos), y = c(.45, .2), xpd=NA)
text(xpos,.15, labels = "Level 3", las=.5, xpd=NA)

 

## ---- eval=F-------------------------------------------------------------
#  summary(workflow)
#  plot(workflow)
#  test(workflow)
#  submit(workflow)
#  res <- collect(workflow)	
#  

## ---- eval=F-------------------------------------------------------------
#  summary(res, .fun = mean, .reps = NULL)  		
#  err(res)
#  warn(res)  		

## ---- eval=F-------------------------------------------------------------
#  workflow <- workflow + layer(grid(f2, arg6 = c(5:8)), .id = 1)
#  
#  submit(workflow)
#  res <- collect(workflow)
#  
#  workflow <- workflow +
#    layer(grid(g3, ..., .id = 2)) +
#   layer(h3(.dep=c("g3"), .id = 3)
#  
#  submit(workflow)
#  res <- collect(workflow)

## ---- echo=F, eval=T, fig.width=8, fig.height=4--------------------------
 par(mar = c(1, 1, 1, 1), mfrow = c(1, 2), oma=c(1,1,1,1))
 names <- c("f()", "f2()", "g()", "g2()", "h()")
M <- matrix(nrow = 5, ncol = 5, byrow = TRUE, data = 0)
M[3, 1:2] <- M[4, 1:2] <-  ""
M[5, 3:4] <- ""

update <- matrix(nrow = 5, ncol = 5, byrow = TRUE, data = "gray")
update[3:4, 2] <- "black"
update[5,3:4] <- "black"

plotmat(M, pos = c(2, 2, 1), name = names, lwd= 1, relsize=1,
         box.lwd = 1, cex.txt=.8, box.type = "circle", box.prop=1.0,
         arr.lcol=update, arr.col=update, box.lcol=c("gray", "black", "black", "black", "black"))

xpos <- .975
text(xpos,.85, labels = "Level 1", las=.5, xpd=NA)
lines(x = c(xpos, xpos), y = c(.8, .55))
text(xpos,.5, labels = "Level 2", las=.5, xpd=NA)
lines(x = c(xpos, xpos), y = c(.45, .2))
text(xpos,.15, labels = "Level 3", las=.5, xpd=NA)

par(mar = c(0, 0, 0, 0))
names <- c("f()", "f2()", "g()", "g2()", "g3()", "h()", "h3()")
M <- matrix(nrow = 7, ncol = 5, byrow = TRUE, data = 0)
M[3, 1:2] <- M[4, 1:2] <-  M[5, 1:2] <- ""
M[6, 3:4] <- M[7, 5] <- ""
update <- matrix(nrow = 7, ncol = 5, byrow = TRUE, data = "gray")
update[5, 1:2] <- update[7, 5] <- "black"

plotmat(M, pos = c(2, 3, 2), name = names, lwd= 1, relsize=1,
         box.lwd = 1, cex.txt=.8, box.type = "circle", box.prop=1.0,
        arr.lcol=update, arr.col=update, 
        box.lcol=c(rep("gray", 4), "black", "gray", "black"))

## ----eval=T, cache=T, messages=F-----------------------------------------

library(gbm)
data("mpg",package="ggplot2") 
is.fac <- sapply(mpg, is.character)
mpg[,is.fac] <- lapply(mpg[,is.fac], as.factor)

do.one <- function(fold, ...){
  set.seed(104)
  fold.ids <- sample(1:5, size =  nrow(mpg), replace=T)
  test <- mpg[fold.ids == fold, ]
  out <- gbm(cty ~ . - hwy, data = mpg[fold.ids != fold, ], distribution="gaussian", ...)
  nt <- suppressWarnings(gbm.perf(out, method="OOB", plot.it=F))
  yhat <- predict(out, newdata = test, n.trees=nt)
  mse <- var(test$cty - yhat)
  return(c(mse=mse))
}

do.one(fold=1, n.trees=5) # test

cv.tune <- gapply(do.one, fold=1:5, n.trees=c(1000, 5000), 
       shrinkage=c(.01, .001, .005), interaction.depth=c(1:3), .mc.cores=5)

library(dplyr)
cv.tune %>% group_by(n.trees, shrinkage, interaction.depth) %>% 
  summarise(mse=mean(value)) %>%
  ungroup %>%
  arrange(mse) %>% head(n=5)



