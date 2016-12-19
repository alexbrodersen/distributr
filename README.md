# distributr
Tidy distributed computing workflows for R and Sun/Open Grid Engine

    devtools::install_github("patr1ckm/distributr")
    
The basic function is `grid_apply`, which applies a function over a grid of its arguments (`expand.grid(...)`), returning results in a list. Function applications can be executed repeatedly and in parallel.

### grid_apply
 
```{r, eval=TRUE}
do.one <- function(n, mu, sd){ mean(rnorm(n, mu, sd)) }

sim <- grid_apply(do.one, n = c(50, 100, 500), mu = c(1,5), sd = c(1, 5, 10), 
              .reps=50, .mc.cores=5)
```

### tidy

A tidy method is provided that merges the list of results with the argument grid, putting the results in tidy (long) form. This format is convenient for plotting and further data analysis. `tidy` works with lists of vectors, lists, and data frames. The function `gapply` runs `grid_apply` followed by `tidy`. 

```{r, eval=TRUE}
res <- sim %>% tidy
res <- gapply(do.one, n = c(50, 100, 500), mu = c(1,5), sd = c(1, 5, 10), 
              .reps=50, .mc.cores=5)
```

### Warnings and Errors

```{r, eval=TRUE}
err(sim)
warn(sim)
```

### Sun/Open Grid Engine

A compute plan can be setup and executed using the Sun/Open Grid Engine scheduler. Rows of the argument grid are submitted to nodes, and replications are carried out in parallel via `mclapply`. 

```{r}
sim <- gapply(do.one, n = c(50, 100, 500), mu = c(1,5), sd = c(1, 5, 10), .eval=F)
sim <- setup(sim, .reps=500, .mc.cores = 5)
submit(sim)   
res <- collect(sim) %>% tidy
```

Jobs can be added and filtered

```{r}
jobs(sim)
add_jobs(sim, n=1000, mu=10, sd=50)
filter_jobs(sim, n < 100, .mc.cores=5)
```
