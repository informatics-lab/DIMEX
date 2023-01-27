test_that("calculate_outdoor", {
  df <- data.frame(pm25 = c(1, 2, 3))
  result <- calculate_outdoor(df)
  expect_equal(result$conc, c(1, 2, 3))
})
