test_that("daily_averages works", {
  pop_id <- 2
  date <- "2021-02-20"
  day_type_label <- "Weekend"
  season_label <- "Winter"
  sex_id <- 0
  sex_label <- "Female"
  agegr4_id <- 5
  activities_complete <- data.frame(act_id = c(0, 1),
                                    pop_id = pop_id,
                                    date = date,
                                    time = 1,
                                    time_label = "04:00-04:10",
                                    activity = 110,
                                    activity_label = "Sleep",
                                    location = 11,
                                    location_label = "Home",
                                    hour = 4,
                                    day_label = "Sunday",
                                    day = 1,
                                    daytype = 1,
                                    daytype_label = day_type_label,
                                    season = 1,
                                    season_label = season_label,
                                    micro_group = "home",
                                    area_id = "",
                                    sex = sex_id,
                                    sex_label = sex_label,
                                    agegr4 = agegr4_id,
                                    agegr4_label = "",
                                    nssec5 = 2,
                                    nssec5_label = "",
                                    pm25_cams_agg = c(1.0, 3.0),
                                    pm25_five = 5,
                                    pm25_emep_agg = 1.0,
                                    pm25_cams_agg_tns = 60.0,
                                    pm25_emep_agg_tns = 40.0,
                                    pm25_five_tns = 30.0,
                                    pm25_cams_agg_inh = 10.0,
                                    pm25_emep_agg_inh = 10.0,
                                    pm25_five_inh = 10.0,
                                    uniid = 1,
                                    pm25_cams_agg_hhd = 10.0,
                                    pm25_emep_agg_hhd = c(10.0, 20.0),
                                    pm25_five_hhd = 10.0)

  expected <- data.frame(area_id = "",
                         pop_id = pop_id,
                         date = date,
                         daytype = 1,
                         daytype_label = day_type_label,
                         season = 1,
                         season_label = season_label,
                         sex = sex_id,
                         sex_label = sex_label,
                         agegr4 = agegr4_id,
                         agegr4_label = "",
                         nssec5 = 2,
                         nssec5_label = "",
                         exposure_cams = 10.0,
                         exposure_emep = 15.0,
                         exposure_five = 10.0,
                         pm25_cams_agg = 2.0,
                         pm25_emep_agg = 1,
                         pm25_five = 5)

  actual <- daily_averages(activities_complete)
  expect_equal(actual, expected)
})
