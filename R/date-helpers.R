#' Type of day in terms of a number
#' 
#' @export
#' @param day - integer between 1 and 7
#' @returns integer representing either week day or weekend
day_type <- function(day) {
  dplyr::case_when(
    day %in% c(1, 7) ~ 1,
    day %in% 2:6 ~ 2
  )
}

#' Type of day, either Weekend or Weekday
#' 
#' @export
#' @param day - integer between 1 and 7
#' @returns string either "Weekend" or "Weekday"
day_type_label <- function(day) {
  dplyr::case_when(
    day_type(day) == 1 ~ "Weekend",
    day_type(day) == 2 ~ "Weekday"
  )
}

#' Day of week in terms of a number
#' 
#' @export
#' @param date - a Date object
#' @returns integer between 1 and 7 inclusive
day_number <- function(date) {
  as.numeric(factor(weekdays(date), levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")))
}

#' Season
#' 
#' @export
#' @param month - integer between 1 and 12 inclusive
#' @returns integer representing season
season <- function(month) {
  dplyr::case_when(
    month %in% c(12, 1, 2) ~ 1,
    month %in% c(3:5) ~ 2,
    month %in% c(6:8) ~ 3,
    month %in% c(9:11) ~ 4
  )
}

#' Season label
#' 
#' @export
#' @param month - integer between 1 and 12 inclusive
#' @returns string of "Winter", "Spring", "Summer" or "Autumn"
season_label <- function(month) {
  x <- season(month)
  dplyr::case_when(
      x == 1 ~ "Winter",
      x == 2 ~ "Spring",
      x == 3 ~ "Summer",
      x == 4 ~ "Autumn"
  )
}

#' Date filter
#' 
#' @export
#' @param frame - data frame to filter
#' @param start_date - start date of date filter interval (inclusive)
#' @param end_date - end date of date filter interval (inclusive)
#' @returns only dates within filter window
date_filter <- function(frame, start_date, end_date) {
  frame <- frame %>%
    dplyr::filter(
      as.Date(date) >= as.Date(start_date) &
      as.Date(date) <= as.Date(end_date)
    )
  frame
}
