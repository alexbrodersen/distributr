
#' Asks for user input if overwrite would invalidate .sge_id
#' @param object grid_apply object
#' @param .dir directory
#' @importFrom utils menu
check_overwrite <- function(object, .dir){
  grid_name <- paste0(.dir, "arg_grid.Rdata")
  res <- 1
  if(file.exists(grid_name) && !isTRUE(check_valid_grid(object, grid_name))){
    question <- "This will overwrite the existing job data base (arg_grid.Rdata), and corrupt results. Are you sure?"
    if(interactive()) {
      res <- menu(c("Yes", "No"), title=question)
    } else {
      stop("This will overwrite the existing job data base (arg_grid.Rdata), and corrupt results.")
    }
  }
  res
}

check_valid_grid <- function(object, grid_name){
  disk_grid <- readRDS(grid_name)
  obj_grid <- jobs(object)
  if(is.null(obj_grid$.sge_id)){
    obj_grid$.sge_id <- 1:nrow(obj_grid)
  }
  all.equal(obj_grid[1:nrow(disk_grid),], disk_grid)
}
