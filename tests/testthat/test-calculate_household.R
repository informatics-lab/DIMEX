test_that("calculate_household", {
  
  # Column names: Activity, micro_group, season_label, housetype
  # micro_group: home
  # activity: 3110, 3100, 4210, or 3190
  # season_label: Winter, Spring, Summer, or Autumn

  ambient <- "pm25"  # NOTE: configurable ambient concentration column name
  act_dat <- data.frame(pop_id = c(0, 0, 1, 1),
                        date = c(0, 1, 2, 3),
                        time = c(0, 1, 2, 3),
                        activity = c(3110, 3110, 3110, 3110),
                        micro_group = c("home", "home", "home", "home"),
                        season = 1,
                        pm25 = c(5, 5, 5, 5))
  pop_dat <- data.frame(pop_id = c(0, 1, 2, 3),
                        housetype = c(0, 1, 2, 3))
  
  outvar <- "bar"
  
  set.seed(1409)
  result <- calculate_household(act_dat, pop_dat, ambient, outvar)
  expected <- c(12.9436723, NaN, 9.5234616, NaN)
  expect_equal(result[, outvar], expected)
})
