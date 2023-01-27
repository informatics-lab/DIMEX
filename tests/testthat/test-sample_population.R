test_that("sample_population", {
  pop_dat <- data.frame()
  tus_dat <- data.frame()
  actual <- sample_population(pop_dat, tus_dat)
  expect_equal(actual, 4)
})
