test_that("estimate_exposures", {
  
  activities_complete <- data.frame(act_id = c(11354, 13473),
                                    pop_id = c(13, 13),
                                    date = as.Date(c("2020-12-20", "2020-12-20")),
                                    time = c(122, 130),
                                    time_label = c("00:10-00:20", "01:30-01:40"),
                                    activity =  c(110, 110),
                                    activity_label = c("Sleep", "Sleep"),
                                    location = c(11, 11),
                                    location_label = c("Home", "Home"),
                                    hour = c(0, 0),
                                    day_label = c("Sunday", "Monday"),
                                    day = c(1, 2),
                                    daytype = c(1, 2),
                                    daytype_label = c("Weekend", "Weekday"),
                                    season = c(1, 1),
                                    season_label = c("Winter", "Winter"),
                                    micro_group = c("home", "home"))
  
  pop_dat <- data.frame(pop_id = 13,
                        housetype = "semi-detached",
                        area_id = "E02000984",
                        agegr4 = 4,
                        agegr4_label = "45-59",
                        sex = 1,
                        sex_label = "Male",
                        nssec5 = 2,
                        nssec5_label = "Intermediate occupations")
  
  pm25_ctm <- data.frame(area_id = "E02000984",
                         date = as.Date("2020-12-20"),
                         hour = 0,
                         pm25_cams_agg = 5,
                         pm25_emep_agg = 5,
                         pm25_five = 5)
  
  set.seed(1409)
  
  actual <- estimate_exposures(activities_complete, pop_dat, pm25_ctm)
  
  actual$pm25_cams_agg_tns <- round(actual$pm25_cams_agg_tns, 2)
  actual$pm25_emep_agg_tns <- round(actual$pm25_emep_agg_tns, 2)
  actual$pm25_five_tns <- round(actual$pm25_five_tns, 2)
  
  actual$pm25_cams_agg_inh <- round(actual$pm25_cams_agg_inh, 2)
  actual$pm25_emep_agg_inh <- round(actual$pm25_emep_agg_inh, 2)
  actual$pm25_five_inh <- round(actual$pm25_five_inh, 2)
  
  actual$pm25_cams_agg_hhd <- round(actual$pm25_cams_agg_hhd, 2)
  actual$pm25_emep_agg_hhd <- round(actual$pm25_emep_agg_hhd, 2)
  actual$pm25_five_hhd <- round(actual$pm25_five_hhd, 2)
  
  
  expected <- data.frame(act_id = c(11354, 13473),
                         pop_id = c(13, 13),
                         date = as.Date(c("2020-12-20", "2020-12-20")),
                         time = c(122, 130),
                         time_label = c("00:10-00:20", "01:30-01:40"),
                         activity =  c(110, 110),
                         activity_label = c("Sleep", "Sleep"),
                         location = c(11, 11),
                         location_label = c("Home", "Home"),
                         hour = c(0, 0),
                         day_label = c("Sunday", "Monday"),
                         day = c(1, 2),
                         daytype = c(1, 2),
                         daytype_label = c("Weekend", "Weekday"),
                         season = c(1, 1),
                         season_label = c("Winter", "Winter"),
                         micro_group = c("home", "home"),
                         area_id = c("E02000984", "E02000984"),
                         sex = c(1, 1),
                         sex_label = c("Male", "Male"),
                         agegr4 = c(4, 4),
                         agegr4_label = c("45-59", "45-59"),
                         nssec5 = c(2, 2),
                         nssec5_label = c("Intermediate occupations", "Intermediate occupations"), 
                         pm25_cams_agg = c(5, 5),
                         pm25_five = c(5, 5),
                         pm25_emep_agg = c(5, 5),
                         pm25_cams_agg_tns = c(28.65, 34.63),
                         pm25_emep_agg_tns = c(33.44, 36.13), 
                         pm25_five_tns = c(25.57, 12.45),
                         pm25_cams_agg_inh = c(11.58, 7.69),
                         pm25_emep_agg_inh = c(7.09, 10.24),
                         pm25_five_inh = c(10.79, 6.36),
                         uniid = c(1, 2),
                         pm25_cams_agg_hhd = c(14.63, 6.60), 
                         pm25_emep_agg_hhd = c(13.42, 8.99),
                         pm25_five_hhd = c(10.08, 5.72))
  
  expect_equal(actual, expected)
  
})
