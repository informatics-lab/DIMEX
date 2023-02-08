#' Categorise micro-environment from location ID
#' 
#' @export
#' @param location - ID indicating location
#' @return micro-environment text representation
micro_group <- function(location) {
  dplyr::case_when(
    location %in% c(11, 12) ~ "home",
    location %in% c(13, 14, 15, 16, 17, 19, 20, 21) ~ "indoor",
    location %in% c(18, 31, 32) ~ "outdoor",
    location %in% c(30, 33:49, 90) ~ "transport"
  )
}