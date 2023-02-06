test_that("estimate_exposures", {
  
  activities_complete <- data.frame(act_id = 11354,
                                    pop_id = 13,
                                    date = as.Date("2020-12-20"),
                                    time = 122,
                                    time_label = "00:10-00:20",
                                    activity = 110,
                                    activity_label = "Sleep",
                                    location = 11,
                                    location_label = "Home",
                                    hour = 0,
                                    day_label = "Sunday",
                                    day = 1,
                                    daytype = 1,
                                    daytype_label = "Weekend",
                                    season = 1,
                                    season_label = "Winter",
                                    micro_group = "home")
  
  pop_dat <- data.frame(pop_id = c(0, 0),
                        housetype = c(0, 0),
                        area_id = c(0,0),
                        sex = c(0,0),
                        sex_label = c("female", "female"),
                        agegr4 = c(1,2),
                        agegr4_label = c("<16", "<16"),
                        nssec5 = c(2,2),
                        nssec5_label = c("Not applicable", "Not applicable"))
  
  pm25_ctm <- data.frame(area_id = c(0,0),
                         date = as.Date(c("2021-01-01", "2021-01-02")),
                         hour = c(0, 0),
                         pm25_cams_agg = c(5, 5),
                         pm25_emep_agg = c(5, 5),
                         pm25_five = c(5, 5))
  
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
