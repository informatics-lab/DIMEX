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