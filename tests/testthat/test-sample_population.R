test_that("sample_population", {
  # Need columns: pop_id, sex, agegr4, nssec5
  #               act_id, time, time_label
  pop_dat <- data.frame(
    pop_id = c(0, 0),
    area_id = c(0, 0),
    sex = c(0, 0),
    agegr4 = c(0, 0),
    nssec5 = c(0, 0)
  )
  tus_dat <- data.frame(
    pop_id = c(0, 0),
    act_id = c(1, 1),
    sex = c(0, 0),
    agegr4 = c(0, 0),
    nssec5 = c(0, 0),
    time = c(0, 0),
    activity = c(0, 0)
  )

  nsample <- 1
  weights <- NULL

  pop_strata <- c("area_id")
  tus_strata <- c("sex")

  start_date <- "2023-01-01"
  end_date <- "2023-01-01"
  keep <- c("activity")

  actual <- sample_population(
    pop_dat,
    tus_dat,
    nsample,
    weights,
    pop_strata,
    tus_strata,
    start_date,
    end_date,
    keep
  )
  expect_equal(actual$pop_id, c(0, 0, 0, 0))
})

test_that("stress test population sampler to understand limitations", {
  sample_size <- 1

  # Synthetic population dataset
  msoa_id <- "FAKE_MSOA_ID"
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
    daytype = 2,
    time = 0,
    activity = 0,
    location = 0
  )

  # System under test
  set.seed(1409)
  a_date <- "2020-11-30"
  actual <- sample_population(population, time_use_survey,
    nsample = sample_size,
    weights = "weights_diary",
    pop_strata = c("area_id"),
    tus_strata = c("sex", "agegr4", "nssec5", "daytype"),
    start_date = a_date,
    end_date = a_date,
    keep = c("activity", "location")
  )

  # Expectation (column order important for data.frame equals)
  date <- as.Date(a_date)
  day <- day_number(date)
  daytype <- day_type(day)
  expected <- data.frame(
    act_id = 1,
    pop_id = 1,
    date = date,
    day = day,
    daytype = daytype,
    season = season(lubridate::month(date)),
    time = 0,
    activity = 0,
    location = 0
  )

  expect_equal(actual[1, ], expected)
  expect_equal(nrow(actual), 1)
})

test_that("stratify_by_column given a single key", {
  frame <- data.frame(foo = c(41, 42, 43))
  column_names <- c("foo")
  actual <- stratify_by_column(frame, column_names)
  expected <- tibble::tibble(
    foo = frame$foo,
    strata = c(1, 2, 3)
  )
  expect_equal(actual, expected)
})

test_that("stratify_by_column given duplicates removes duplicates", {
  duplicate_foo <- c(1, 2, 3, 3)
  unique_foo <- c(1, 2, 3)
  frame <- data.frame(foo = duplicate_foo)
  column_names <- c("foo")
  actual <- stratify_by_column(frame, column_names)
  expected <- tibble::tibble(
    foo = unique_foo,
    strata = unique_foo
  )
  expect_equal(actual, expected)
})

test_that("stratify_by_column given multiple keys", {
  # Unique combinations of strata keys are kept
  frame <- data.frame(foo = c(12, 13, 13), bar = c("A", "B", "C"))
  column_names <- c("foo", "bar")
  actual <- stratify_by_column(frame, column_names)
  expected <- tibble::tibble(
    foo = frame$foo,
    bar = frame$bar,
    strata = c(1, 2, 3)
  )
  expect_equal(actual, expected)
})
