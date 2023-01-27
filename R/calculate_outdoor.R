#' A function to calculate the outdoor air pollution
#'
#' @param dat - Dataset containing sampled population with 
#'              outdoor activities
#' @return Dataset containing the sampled PM2.5 estimate
#'         for outdoors
#' @export
calculate_outdoor <- function(dat) {
  # Parameters
  a <- 0
  b <- 1
  # Estimating ambient 
  dat$conc <- a + b * dat$pm25
  # Returning dataset
  return(dat)
}
