test_that("fill_in_missings works", {
  # Define minimal data.frame that satisfies all of the pipe operations
  # column names: sex, agegr4, nssec5, daytype, activity, location
  location <- c(11, 11, 11)  # Home
  sex <- c(0, 0, 0)
  agegr4 <- c(0, 0, 0)
  nssec5 <- c(0, 0, 0)
  daytype <- c(0, 0, 0)
  activity <- c(0, 0, 0)
  frame <- data.frame(location = location,
                      sex = sex,
                      agegr4 = agegr4,
                      nssec5 = nssec5,
                      daytype = daytype,
                      activity = activity)
  actual <- fill_in_missings(frame)
  expected <- c("Home", "Home", "Home")
  expect_equal(actual$location_popular_label, expected)
})


test_that("snippet from fill_in_missings converts missing values to NA", {
  location <- c(1, 2, 3, -9)  # missing values -9, 0, 10 or 99
  frame <- data.frame(location = location)
  actual <- frame %>%
    dplyr::mutate(location_popular = ifelse(location %in% c(-9, 0, 10, 99), NA, location))
  expected <- data.frame(location = location,
                         location_popular = c(1, 2, 3, NA))
  expect_equal(actual, expected)
})

test_that("uktus_metadata_location shipped with code", {
  expect_true(exists("uktus_metadata_location"))
  expect_equal(uktus_metadata_location$location, c(0, 10:21, 30:49, 90, 99, -9, -7, -2))
  expect_equal(uktus_metadata_location$location_label[1:3], c(
    "Unspecified location",
    "Unspecified location (not travelling)",
    "Home"
  ))
})