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
