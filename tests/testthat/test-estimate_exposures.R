test_that("estimate_exposures works", {
  
  # Example activities data frame including all columns (all required)
  activities_complete <- data.frame(act_id = c(11354, 13473),
                                    pop_id = c(13, 13),
                                    date = as.Date(c("2020-12-20", "2020-12-20")),
                                    time = c(122, 130),
                                    activity =  c(110, 110),
                                    location = c(11, 11),
                                    hour = c(0, 0),
                                    day = c(1, 2),
                                    daytype = c(1, 2),
                                    season = c(1, 1),
                                    micro_group = c("home", "home"))
  
  # Example population data frame containing only required columns
  pop_dat <- data.frame(pop_id = 13,
                        housetype = "semi-detached",
                        area_id = "E02000984",
                        agegr4 = 4,
                        sex = 1,
                        nssec5 = 2)
  
  # Example merged EMEP and CAMS PM2.5 data frame containing all columns (all required)
  pm25_ctm <- data.frame(area_id = "E02000984",
                         date = as.Date("2020-12-20"),
                         hour = 0,
                         pm25_cams_agg = 5,
                         pm25_emep_agg = 5,
                         pm25_five = 5)
  
  # Setting the seed
  set.seed(1409)
  
  actual <- estimate_exposures(activities_complete, pop_dat, pm25_ctm)
  
  # Change results to two decimal places in order to test against expected values
  exposures <- c("pm25_cams_agg_tns", "pm25_emep_agg_tns", "pm25_five_tns",
                 "pm25_cams_agg_inh", "pm25_emep_agg_inh", "pm25_five_inh",
                 "pm25_cams_agg_hhd", "pm25_emep_agg_hhd", "pm25_five_hhd")
  
  actual[, exposures] <- round(actual[, exposures], 2)
  
  expected <- data.frame(act_id = c(11354, 13473),
                         pop_id = c(13, 13),
                         date = as.Date(c("2020-12-20", "2020-12-20")),
                         time = c(122, 130),
                         activity =  c(110, 110),
                         location = c(11, 11),
                         hour = c(0, 0),
                         day = c(1, 2),
                         daytype = c(1, 2),
                         season = c(1, 1),
                         micro_group = c("home", "home"),
                         area_id = c("E02000984", "E02000984"),
                         sex = c(1, 1),
                         agegr4 = c(4, 4),
                         nssec5 = c(2, 2),
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
