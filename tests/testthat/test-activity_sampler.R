test_that("activity_sampler works", {
  # Minimal population data.frame
  msoa_id = 2
  n <- 100
  population <- data.frame(
    area_id = rep(msoa_id, each = n),  # Sample size is hard-coded to 100
    pop_id = c(1:n),
    sex = rep(0, each = n),
    agegr4 = rep(0, each = n),
    nssec5 = rep(0, each = n)
  )
  
  # Minimal Time-Use Survey data.frame
  # This is a very complex spec to track down by just reading the code
  tus_dat <- data.frame(percmissing = rep(0, each = n),
                        weights_diary = rep(1, each = n),
                        sex = rep(0, each = n),
                        agegr4 = rep(0, each = n),
                        nssec5 = rep(0, each = n),
                        pop_id = c(1:n),
                        act_id = rep(1, each = n),
                        daytype = rep(1:7, each = 15)[1:100],
                        time = rep(0, each = n),
                        time_label = rep(0, each = n),
                        activity = rep(0, each = n),
                        activity_label = rep(0, each = n),
                        location = rep(0, each = n),
                        location_label = rep(0, each = n)
                        )
  
  # System under test  
  actual <- activity_sampler(population, tus_dat, msoa_id)

  # Assertions
  ones <- rep(1, each = 100)
  expected <- data.frame(
    act_id = ones,
    pop_id = ones,
    date = ones,
    time = ones,
    time_label = ones,
    activity = ones,
    activity_label = ones,
    location = ones,
    location_label = ones,
    hour = ones,
    day_label = ones,
    day = ones,
    daytype = ones,
    daytype_labe = ones,
    season = ones,
    season_label = ones,
    micro_group = ones
  )

  expect_equal(actual$act_id, expected$act_id)
  # expect_equal(actual, expected)
})
