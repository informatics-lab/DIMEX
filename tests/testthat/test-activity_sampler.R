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
  # This is a very complex spec to track down by just reading the code
  n <- 100
  tus_dat <- data.frame(percmissing = rep(0, each = n),
                        weights_diary = rep(1, each = n),
                        sex = rep(0, each = n),
                        agegr4 = rep(0, each = n),
                        nssec5 = rep(0, each = n),
                        daytype = rep(0, each = n),
                        pop_id = rep(1, each = n),
                        act_id = rep(1, each = n),
                        time = rep(0, each = n),
                        time_label = rep(0, each = n),
                        activity = rep(0, each = n),
                        activity_label = rep(0, each = n)
                        )
  
  # System under test  
  actual <- activity_sampler(population, tus_dat, msoa_id)

  expect_equal(actual, 4)
})
