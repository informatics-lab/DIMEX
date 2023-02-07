make_time_use_survey <- function(date,
                                 percent_missing = 0, weights = 1, sex = 1, age = 1,
                                 socio_economic_status = 1) {
  weekday_or_weekend <- day_type(day_number(date))
  data.frame(
    percmissing = percent_missing,
    weights_diary = weights,
    sex = sex,
    agegr4 = age,
    nssec5 = socio_economic_status,
    daytype = weekday_or_weekend
  )
}

make_population <- function(msoa) {
  data.frame(
    area_id = msoa,
    pop_id = 1,
    sex = 1,
    agegr4 = 6,
    nssec5 = 5
  )
}

test_that("activity_sampler given a single row", {
  # What is this test trying to show?
  skip("purpose of this test not clear")

  msoa <- "E02000984"
  date_str <- "2021-01-01"
  date <- as.Date(date_str)
  sample_size <- 2

  # System under test
  activities <- activity_sampler(make_population(msoa), make_time_use_survey(date), msoa,
    start_date = date_str,
    end_date = date_str,
    sample_size = sample_size
  )

  # Assertions
  actual <- activities
  expected <- data.frame()
  expect_equal(actual, expected)
})

test_that("activity_sampler works", {
  # WARNING: This is a long running test

  # Minimal population data.frame
  msoa_id <- 2
  sample_size <- 21 # Related to days in week for now
  start_date <- "2020-12-01"
  end_date <- "2021-12-31"

  population <- data.frame(
    area_id = msoa_id,
    pop_id = c(1:sample_size),
    sex = 0,
    agegr4 = 0,
    nssec5 = 0
  )

  # Minimal Time-Use Survey data.frame
  # This is a very complex spec to track down by just reading the code
  tus_dat <- data.frame(
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
  actual <- activity_sampler(population,
    tus_dat, msoa_id,
    start_date = start_date,
    end_date = end_date,
    sample_size = sample_size
  )

  # Assertions
  n_samples_magic_number <- 27006 # Sensitive to seed and sample_size
  ones <- rep(1, each = n_samples_magic_number)

  # NOTE: these are not the actual values of the activity_sampler
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
    daytype_label = ones,
    season = ones,
    season_label = ones,
    micro_group = ones
  )

  # NOTE: Too many asserts is a code smell. It would be better if the system
  #       under test produced a more intuitive result
  expect_equal(nrow(actual), n_samples_magic_number)
  expect_equal(colnames(actual), colnames(expected))
  expect_equal(actual$act_id, expected$act_id)
  # expect_equal(actual, expected)
})

test_that("sample_sequences works", {
  # NOTE: sampling blows up if a strata only has one row and the activity_id
  #       is not 1, which leads to sample(x = 2, size = 1, prob = 1) issue
  # Activities
  strata <- c(2)
  population_id <- c(2)
  activities <- data.frame(
    strata = strata,
    pop_id = population_id
  )

  # Time use survey
  weights <- c(1, 0.5, 0.5, 1)
  strata <- c(1, 2, 2, 3)
  activity_id <- c(1, 2, 2, 3)
  time_use_survey_activities <- data.frame(
    weights = weights,
    strata = strata,
    act_id = activity_id
  )

  # System under test
  actual <- sample_sequences(activities, time_use_survey_activities)
  expected <- data.frame(strata=c(2), pop_id=c(2), act_id=c(2))
  expect_equal(actual, expected)
})
