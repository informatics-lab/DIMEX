test_that("activity_sampler works", {
  # Minimal population data.frame
  msoa_id = 2
  population <- data.frame(
    area_id = rep(msoa_id, each = 100),  # Sample size is hard-coded to 100
    pop_id = c(1:100),
    sex = rep(0, each = 100),
    agegr4 = rep(0, each = 100),
    nssec5 = rep(0, each = 100)
  )
  
  # Minimal Time-Use Survey data.frame
  tus_dat <- data.frame(percmissing = c(0, 0),
                        weights_diary = c(1, 1),
                        sex = c(0, 0),
                        agegr4 = c(0, 0),
                        nssec5 = c(0, 0),
                        daytype = c(0, 0)
                        )
  
  # System under test  
  actual <- activity_sampler(population, tus_dat, msoa_id)

  expect_equal(actual, 4)
})
