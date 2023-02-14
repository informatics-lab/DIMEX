test_that("merge_pollutants functions", {
  start_date <- "2021-01-01"
  end_date <- "2021-03-01"
  row_date <- "2021-01-05"
  pm25_cams <- data.frame(
    area_id = "E02000984",
    date = row_date,
    hour = 0,
    pm25_cams_agg = c(10, 12)
  )
  pm25_emep <- data.frame(
    area_id = "E02000984",
    date = row_date,
    hour = 0,
    pm25_cams_agg = c(11, NA)
  )

  expected <- data.frame(
    area_id = "E02000984",
    date = as.Date(row_date),
    hour = 0,
    pm25_cams_agg = c(10, 10, 12, 12),
    pm25_emep_agg = c(11, 10, 11, 12),
    pm25_five = 5
  )
  actual = merge_pollutants(pm25_cams, pm25_emep, start_date, end_date)
  expect_equal(actual, expected)
})
