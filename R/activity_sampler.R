#' Sampling activity sequences (Method 1) - Complete sequences only
#'
#' @param pop_dat - population data frame
#' @param tus_dat - time-use survey data frame
#' @param k - key to index chunk
#' @return data frame of sampled activities
#'
#' @export
activity_sampler <- function(
    pop_dat, tus_dat, k,
    start_date = "2020-12-01",
    end_date = "2021-12-31",
    sample_size = 100) {

  # TODO: Understand time filtering
  #       - 04:00 - 04:00 is addressed here by starting a day early
  #       - Mass-balance equilibrium needs a spin-up time
  #         not accounted for here
  day_before <- as.Date(start_date) - lubridate::days(1)

  # NOTE: Could the sampled population be passed in?
  activities_complete <- sample_population(subset(pop_dat, area_id == k),
    subset(tus_dat, percmissing == 0),
    nsample = sample_size,
    weights = "weights_diary",
    pop_strata = c("area_id"),
    tus_strata = c("sex", "agegr4", "nssec5", "daytype"),
    start_date = day_before,
    end_date = end_date,
    keep = c("activity", "location")
  )

  # Activity sequences run from 04:00-03:59 so need to "shift"
  # the end of the profiles into the next day
  activities_complete <- activities_complete %>%
    # Getting hour and resting date after 00:00
    dplyr::mutate(
      hour = (floor((time - 1) / 6) + 4) %% 24,
      date = dplyr::if_else(hour %in% 0:3, date + 1, date)
    ) %>%
    # Removign day information as we have to shift the day
    dplyr::select(-c(day, daytype, season)) %>%
    # Adding on day information
    dplyr::mutate(
      day = day_number(date),
      daytype = day_type(day),
      season = season(lubridate::month(date)),
    ) %>%
    # Removing first day
    date_filter(start_date, end_date)

  # Adding micro-environments to the dataset
  activities_complete <- activities_complete %>%
    dplyr::mutate(micro_group = micro_group(location))

  # Aggregating to the hourly time series
  activities_complete <- activities_complete %>%
    # Getting time within hour
    dplyr::mutate(minutes = ((time - 1) %% 6) + 1) %>%
    # Grouping by population, date and hour to reduce
    dplyr::group_by(pop_id, date, hour) %>%
    # Sample prop to the environments
    dplyr::mutate(sample = sample(1:6, size = 1)) %>%
    # Only keep sampled time point
    dplyr::filter(minutes == sample) %>%
    # Removing unecesary columns
    dplyr::select(-c(minutes, sample))

  activities_complete
}
