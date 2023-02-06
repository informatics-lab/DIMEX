test_that("estimate_exposures", {
  
  activities_complete <- data.frame(act_id = c(11354, 11354),
                                    pop_id = c(13, 13),
                                    date = as.Date(c("2020-12-20", "2020-12-20")),
                                    time = c(122, 130),
                                    time_label = c("00:10-00:20", "01:30-01:40"),
                                    activity =  c(110),
                                    activity_label = c("Sleep", "Sleep"),
                                    location = c(11, 11),
                                    location_label = c("Home", "Home"),
                                    hour = c(0, 1),
                                    day_label = c("Sunday", "Sunday"),
                                    day = c(1, 1),
                                    daytype = c(1, 1),
                                    daytype_label = c("Weekend", "Weekend"),
                                    season = c(1, 1),
                                    season_label = c("Winter", "Winter"),
                                    micro_group = c("home", "home"))
  
  pop_dat <- data.frame(pop_id = c(0),
                        housetype = c(0),
                        area_id = c(0),
                        sex = c(0),
                        sex_label = c("female"),
                        agegr4 = c(1),
                        agegr4_label = c("<16"),
                        nssec5 = c(2),
                        nssec5_label = c("Not applicable"))
  
  pm25_ctm <- data.frame(area_id = c(0),
                         date = as.Date(c("2021-01-01")),
                         hour = c(0),
                         pm25_cams_agg = c(5),
                         pm25_emep_agg = c(5),
                         pm25_five = c(5))
  
  actual <- estimate_exposures(activities_complete, pop_dat, pm25_ctm)
  
  actual_colnames <- colnames(actual)
  
  expected_colnames <- c("act_id", "pop_id", "date", "time", "time_label", 
                         "activity", "activity_label", "location", "location_label","hour", 
                         "day_label", "day", "daytype", "daytype_label", "season", 
                         "season_label", "micro_group", "area_id", "sex", "sex_label", 
                         "agegr4", "agegr4_label", "nssec5", "nssec5_label", "pm25_cams_agg", 
                         "pm25_five", "pm25_emep_agg", "pm25_cams_agg_tns", "pm25_emep_agg_tns", "pm25_five_tns", 
                         "pm25_cams_agg_inh", "pm25_emep_agg_inh", "pm25_five_inh", "uniid", "pm25_cams_agg_hhd", 
                         "pm25_emep_agg_hhd", "pm25_five_hhd")
  
  expect_equal(actual_colnames, expected_colnames)
  
})
