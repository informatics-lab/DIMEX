test_that("sample_population", {
  
  # Need columns: pop_id, sex, agegr4, nssec5
  pop_dat <- data.frame(foo = c(1),
                        pop_id = c(0),
                        sex = c(0),
                        agegr4 = c(0),
                        nssec5 = c(0))
  tus_dat <- data.frame(bar = c(1))
  
  nsample <- 1
  weights <- NULL
  
  pop_strata <- c("foo")
  tus_strata <- c("bar")
  
  start_date <- "2023-01-01"
  end_date <- "2023-01-01"

  actual <- sample_population(pop_dat,
                              tus_dat,
                              nsample,
                              weights,
                              pop_strata,
                              tus_strata,
                              start_date,
                              end_date)
  expect_equal(actual, 4)
})
