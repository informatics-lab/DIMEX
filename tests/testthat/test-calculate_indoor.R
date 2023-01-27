test_that("multiplication works", {
  dat <- data.frame(foo = c(1:3))
  ambient <- "foo"
  outvar <- "bar"
  
  # System under test
  set.seed(1409)
  result <- calculate_indoor(dat, ambient, outvar)
  
  # Expected artificial result
  expected <- c(4.67245851, 9.39628572, 8.92024296)
  expect_equal(result[,outvar], expected)
})
