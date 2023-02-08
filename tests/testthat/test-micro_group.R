test_that("micro_group returns home", {
  location <- c(11, 12)
  actual <- micro_group(location)
  expected <- c("home", "home")
  expect_equal(actual, expected)
})

test_that("micro_group returns indoor", {
  # NOTE: not 18
  location <- c(13, 14, 15, 16, 17, 19, 20, 21)
  actual <- micro_group(location)
  expected <- rep("indoor", each = length(location))
  expect_equal(actual, expected)
})

test_that("micro_group returns outdoor", {
  location <- c(18, 31, 32)
  actual <- micro_group(location)
  expected <- rep("outdoor", each = length(location))
  expect_equal(actual, expected)
})

test_that("micro_group returns transport", {
  location <- c(30, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 90)
  actual <- micro_group(location)
  expected <- rep("transport", each = length(location))
  expect_equal(actual, expected)
})