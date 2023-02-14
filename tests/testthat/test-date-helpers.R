test_that("day_type maps days correctly", {
  days <- c(1, 2, 3, 4, 5, 6, 7)
  expected <- c(1, 2, 2, 2, 2, 2, 1)
  actual <- day_type(days)
  expect_equal(actual, expected)
})

test_that("day_type handles bad data", {
  expect_true(is.na(day_type(0)))
  expect_true(is.na(day_type(8)))
})

test_that("day_number", {
  dates <- seq(as.Date("2021-01-03"), by = "day", length.out = 7)
  actual <- day_number(dates)
  expected <- c(1, 2, 3, 4, 5, 6, 7)
  expect_equal(actual, expected)
  expect_equal(dates %>% weekdays, c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
})

test_that("season", {
  months <- lubridate::month(c(1:12))
  actual <- season(months)
  expected <- c(1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 1)
  expect_equal(actual, expected)
})

test_that("season_label", {
  dates <- seq(as.Date("2021-01-01"), by = "month", length.out = 12)
  actual <- season_label(lubridate::month(dates))
  expected <- c("Winter",
    "Winter",
    "Spring",
    "Spring",
    "Spring",
    "Summer",
    "Summer",
    "Summer",
    "Autumn",
    "Autumn",
    "Autumn",
    "Winter"
  )
  expect_equal(actual, expected)
})

test_that("date_filter on data frame", {
  frame <- data.frame(
    date = format(seq(as.Date("2021-01-01"), by = "month", length.out = 12), "%Y-%m-%d"),
    value = seq(1, length.out = 12)
  )
  start_date <- as.Date("2021-03-01")
  end_date <- as.Date("2021-09-01")
  actual <- frame %>% date_filter(start_date, end_date)
  expect_equal(nrow(actual), 7)
})
