# Type of day in terms of a number
day_type <- function(day) {
  dplyr::case_when(
    day %in% c(1, 7) ~ 1,
    day %in% 2:6 ~ 2
  )
}

# Type of day, either Weekend or Weekday
day_type_label <- function(day) {
  dplyr::case_when(
    day_type(day) == 1 ~ "Weekend",
    day_type(day) == 2 ~ "Weekday"
  )
}

# Day of week in terms of a number
day_number <- function(date) {
  as.numeric(factor(weekdays(date), levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")))
}

# Season
season <- function(month) {
  dplyr::case_when(
    month %in% c(12, 1, 2) ~ 1,
    month %in% c(3:5) ~ 2,
    month %in% c(6:8) ~ 3,
    month %in% c(9:11) ~ 4
  )
}

# Season label
season_label <- function(month) {
  dplyr::case_when(
    season(month) == 1 ~ "Winter",
    season(month) == 2 ~ "Spring",
    season(month) == 3 ~ "Summer",
    season(month) == 4 ~ "Autumn"
  )
}

