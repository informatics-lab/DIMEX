test_that("daily_averages works", {
  activities_complete <- data.frame()

  expected <- data.frame()
  actual <- daily_averages(activities_complete)
  expect_equal(actual, expected)
})
