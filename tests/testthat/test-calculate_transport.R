test_that("calculate_transport works", {
  
  # Data frame containing example CAMS data
  dat <- data.frame(act_id = c(1, 2, 3),
                    pm25_cams_agg = c(1, 5, 15))
  
  ambient <- "pm25_cams_agg"
  
  outvar <- "pm25_cams_agg_tns"
  
  # Set the seed
  set.seed(1409)
  
  actual <- calculate_transport(dat, ambient, outvar)
  
  expected <- data.frame(act_id = c(1, 2, 3),
                         pm25_cams_agg = c(1, 5, 15),
                         pm25_cams_agg_tns = c(25.3197662, 41.0560893, 40.9603636))
  
  expect_equal(actual, expected)
})
