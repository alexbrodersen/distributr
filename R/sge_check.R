
#' Asks for user input if overwrite would invalidate .sge_id
check_overwrite <- function(object, .dir){
  grid_name <- paste0(.dir, "arg_grid.Rdata")
  res <- 1
  if(file.exists(grid_name) && !isTRUE(check_valid_grid(object, grid_name))){
    question <- "This will overwrite the existing job data base (arg_grid.Rdata), and corrupt results. Are you sure?"
    res <- menu(c("Yes", "No"), title=question)
  }
  res
}

check_valid_grid <- function(object, grid_name){
  load(grid_name)
  disk_grid <- arg_grid
  obj_grid <- jobs(object)
  if(is.null(obj_grid$.sge_id)){
    obj_grid$.sge_id <- 1:nrow(obj_grid)
  }
  all.equal(obj_grid[1:nrow(disk_grid),], disk_grid)
}
