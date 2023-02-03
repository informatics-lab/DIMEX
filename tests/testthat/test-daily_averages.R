test_that("daily_averages works", {
  pm25_cams <- data.frame()
  pm25_emep <- data.frame()
  pm25_five <- data.frame()
  activities_complete <- data.frame()

  expected <- data.frame()
  expect_equal(DIMEX::daily_averages(activities_complete), expected)
})
