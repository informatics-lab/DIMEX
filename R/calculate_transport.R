#' A function to calculate the transportation air pollution
#'
#' @param dat - Dataset containing sampled population with 
#'              transportation activities
#' @return Dataset containing the sampled PM2.5 estimate
#'         for indoor not home
#' @export
calculate_transport <- function(dat, ambient, outvar) {
  # parameters (Normal from Burke et al. (2001) truncated)
  a <- truncnorm::rtruncnorm(n = nrow(dat), a = 0, mean = 33, sd = 7.2)
  b <- truncnorm::rtruncnorm(n = nrow(dat), a = 0, mean = 0.26, sd = 0.14)
  # Estimating ambient 
  dat[,outvar] <- truncnorm::rtruncnorm(n = 1, a = 0, mean = a + b * dat[,ambient, drop = TRUE], sd = 12)
  # Returning dataset
  return(dat)
}
