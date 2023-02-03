test_that("sample_population", {
  
  # Need columns: pop_id, sex, agegr4, nssec5
  #               act_id, time, time_label
  pop_dat <- data.frame(pop_id = c(0, 0),
                        area_id = c(0, 0),
                        sex = c(0, 0),
                        agegr4 = c(0, 0),
                        nssec5 = c(0, 0))
  tus_dat <- data.frame(pop_id = c(0, 0),
                        act_id = c(1, 1),
                        sex = c(0, 0),
                        agegr4 = c(0, 0),
                        nssec5 = c(0, 0),
                        time = c(0, 0),
                        time_label = c(0, 0),
                        activity = c(0, 0),
                        activity_label = c(0, 0))
  
  nsample <- 1
  weights <- NULL
  
  pop_strata <- c("area_id")
  tus_strata <- c("sex")
  
  start_date <- "2023-01-01"
  end_date <- "2023-01-01"
  keep <- c('activity', 'activity_label')

  actual <- sample_population(pop_dat,
                              tus_dat,
                              nsample,
                              weights,
                              pop_strata,
                              tus_strata,
                              start_date,
                              end_date,
                              keep)
  expect_equal(actual$pop_id, c(0, 0, 0, 0))
})

test_that("comfirm sample_population behaviour given realistic inputs", {
  sample_size <- 21
  msoa_id <- "FOO"
  population <- data.frame(
    area_id = msoa_id,
    pop_id = c(1:sample_size),
    sex = 0,
    agegr4 = 0,
    nssec5 = 0
  )

  # Linked to population data via pop_id
  time_use_survey <- data.frame(
    percmissing = 0,
    weights_diary = 1,
    sex = 0,
    agegr4 = 0,
    nssec5 = 0,
    pop_id = c(1:sample_size),
    act_id = 1,
    daytype = rep(1:7, each = 15)[1:sample_size],
    time = 0,
    time_label = 0,
    activity = 0,
    activity_label = 0,
    location = 0,
    location_label = 0
  )

  # System under test
  set.seed(1409)
  actual <- sample_population(population, time_use_survey,
    nsample = sample_size,
    weights = "weights_diary",
    pop_strata = c("area_id"),
    tus_strata = c("sex", "agegr4", "nssec5", "daytype"),
    start_date = "2020-11-30",
    end_date = "2021-11-30",
    keep = c("activity", "activity_label", "location", "location_label")
  )

  # Expectation (column order important for data.frame equals)
  expected <- data.frame(
    act_id = 1,
    pop_id = 1,
    date = as.Date("2020-11-30"),
    day_label = "Monday",
    day = 2,
    daytype = 2,
    daytype_label = "Weekday",
    season = 4,
    season_label = "Autumn",
    time = 0,
    time_label = 0,
    activity = 0,
    activity_label = 0,
    location = 0,
    location_label = 0
  )

  expect_equal(actual[1,], expected)
  expect_equal(nrow(actual), 161406)
})
