test_that("fill_in_missings works", {
  location <- c(0)  # valid choices -9, 0, 10 or 99
  tus_dat <- data.frame(location = location)
  # expect_equal(fill_in_missings(tus_dat), 4)
})


test_that("popular locations functionality", {
  location <- c(1, 2, 3)
  frame <- data.frame(location = location)
  actual <- frame %>%
    dplyr::mutate(location_popular = ifelse(location %in% c(-9, 0, 10, 99), NA, location))
  expected <- data.frame(location = location,
                         location_popular = c(1, 2, 3))
  expect_equal(actual, expected)
})
