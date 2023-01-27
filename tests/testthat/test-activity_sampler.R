test_that("activity_sampler works", {
  pop_dat <- data.frame()
  tus_dat <- data.frame(percmissing = c(0),
                        weights = c(1))
  expect_equal(activity_sampler(pop_dat, tus_dat, 0), 4)
})
