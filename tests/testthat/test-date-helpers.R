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