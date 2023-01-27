#' A function to calculate the indoor not home air pollution
#'
#' @param dat - Dataset containing sampled population with 
#'              indoor not home activities
#' @return Dataset containing the sampled PM2.5 estimate
#'         for indoor not home
#' @export
calculate_indoor <- function(dat, ambient, outvar) {
  # parameters (Normal from Burke et al. (2001) truncated)
  a <- truncnorm::rtruncnorm(n = nrow(dat), a = 0, mean = 6.467, sd = 2.1)
  b <- truncnorm::rtruncnorm(n = nrow(dat), a = 0, mean = 0.507, sd = 0.11)
  # Estimating ambient 
  dat[,outvar] <- truncnorm::rtruncnorm(n = 1, a = 0, mean = a + b * dat[,ambient, drop = TRUE], sd = 3.467)
  # Returning dataset
  return(dat)
}
