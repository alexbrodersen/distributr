[![Travis-CI Build Status](https://travis-ci.org/patr1ckm/distributr.svg?branch=master)](https://travis-ci.org/patr1ckm/distributr)
# distributr
Tidy distributed grid search in R and Sun/Open Grid Engine

    devtools::install_github("patr1ckm/distributr")
    
The basic function is `grid_apply`, which applies a function over a grid of its arguments `expand.grid(...)`, returning results in a list. Function applications can be executed repeatedly and in parallel.

## grid_apply
 
```{r, eval=TRUE}
do.one <- function(n, mu, sd){ mean(rnorm(n, mu, sd)) }

sim <- grid_apply(do.one, n = c(50, 100, 500), mu = c(1,5), sd = c(1, 5, 10), 
              .reps=50, .mc.cores=5)
```

```
[[1]]
[1] 1.053669

[[2]]
[1] 1.244468

[[3]]
[1] 1.267939

[[4]]
[1] 1.157546

[[5]]
[1] 0.786027
```

The arguments to grid over must be scalar. Other arguments (such as data) can be passed as a list to `.args`.

## tidy

A tidy method is provided that merges the list of results with the argument grid, putting the results in tidy form. This format is convenient for plotting and further data analysis. `tidy` works with lists of vectors, lists, and data frames. 

The function `gapply` runs `grid_apply` followed by `tidy`. 

```{r, eval=TRUE}
res <- sim %>% tidy
res <- gapply(do.one, n = c(50, 100, 500), mu = c(1,5), sd = c(1, 5, 10), 
              .reps=50, .mc.cores=5)
```

```{r}
   n mu sd .rep     value
1 50  1  1    1 0.9476228
2 50  1  1    2 0.7545730
3 50  1  1    3 0.9154810
4 50  1  1    4 1.0704074
5 50  1  1    5 0.9840148
6 50  1  1    6 1.1933439
```
If results are of varying length, it can be helpful to stack them into `key, value` pairs
with `tidy(., stack=TRUE)`.

## Warnings and Errors

`gapply` captures both warnings and errors. These can be accessed very simply:

```{r, eval=TRUE}
err(sim)
warn(sim)
```

## Sun/Open Grid Engine

A compute plan can be setup and executed using the Sun/Open Grid Engine scheduler. Rows of the argument grid are submitted to nodes, and replications are carried out in parallel via `mclapply`. 

```{r}
sim <- gapply(do.one, n = c(50, 100, 500), mu = c(1,5), sd = c(1, 5, 10), .eval=F)
sim <- setup(sim, .reps=500, .mc.cores = 5)
submit(sim)   
res <- tidy(collect(sim))
```
The `setup` function asks for user confirmation if an existing argument grid would be overwritten. 

### Job Access, Adding jobs, Selecting a subset of jobs

Jobs can be added to the compute plan via `add_jobs`. A set of jobs can be selected from the argument grid using `filter_jobs` and the usual dplyr syntax to `filter`.

```{r}
jobs(sim)                              # access jobs grid (argument grid)
add_jobs(sim, n=1000, mu=10, sd=50)    # add jobs to plan
filter_jobs(sim, n < 100, .mc.cores=5) # filter jobs as in dplyr
collect(sim, filter="n < 100")         # collect results from jobs matching filter
```

### More Information

More information is available on the [wiki](https://github.com/patr1ckm/distributr/wiki), for example, illustrating how chunking, random number control, and caching can all be done transparently via `do.one`. These [slides](http://rpubs.com/patr1ckm/distributr) give a general overview.
