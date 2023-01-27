#' Sampling activity sequences (Method 1) - Complete sequences only
#'
#' @param k - key to index chunk
#' @return data frame of sampled activities
#'
#' @export
activity_sampler <- function(k) {
  # TODO: Remove dependence on global variables, e.g. pop_dat, tus_dat
  activities_complete <- sample_population(subset(pop_dat, area_id == k), 
                                           subset(tus_dat, percmissing == 0), 
                                           nsample = 100,
                                           weights = "weights_diary",
                                           pop_strata = c('area_id'),
                                           tus_strata = c('sex', 'agegr4', 'nssec5', 'daytype'),
                                           start_date = '2020-11-30',
                                           end_date = '2021-12-31',
                                           keep = c('activity', 'activity_label', 'location', 'location_label'))
  
  # Activity sequences run from 04:00-03:59 so need to "shift"
  # the end of the profiles into the next day
  activities_complete <- activities_complete %>%
    # Getting hour and resting date after 00:00
    mutate(hour = (floor((time-1)/6) + 4) %% 24,
           date = if_else(hour %in% 0:3, date + 1, date)) %>%
    # Removign day information as we have to shift the day
    dplyr::select(-c(day, day_label, daytype, daytype_label, season, season_label)) %>%
    # Adding on day information
    mutate(day_label = weekdays(date),
           day = as.numeric(factor(weekdays(date), levels = c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'))), 
           daytype = case_when(day %in% c(1,7) ~ 1,
                               day %in% 2:6 ~ 2),
           daytype_label = case_when(day %in% c(1,7) ~ 'Weekend',
                                     day %in% 2:6 ~ 'Weekday'),
           season = case_when(month(date) %in% c(12, 1, 2) ~ 1,
                              month(date) %in% c(3:5) ~ 2,
                              month(date) %in% c(6:8) ~ 3,
                              month(date) %in% c(9:11) ~ 4),
           season_label = case_when(month(date) %in% c(12, 1, 2) ~ 'Winter',
                                    month(date) %in% c(3:5) ~ 'Spring',
                                    month(date) %in% c(6:8) ~ 'Summer',
                                    month(date) %in% c(9:11) ~ 'Autumn')) %>%
    # Removing first day
    dplyr::filter(date >= as.Date('2020-12-01') & date <= as.Date('2021-12-31'))
  
  # Adding micro-environments to the dataset 
  activities_complete <- activities_complete %>%
    dplyr::mutate(micro_group = case_when(location %in% c(11:12) ~ "home",
                                          location %in% c(13, 14, 15, 16, 17, 19, 20, 21) ~ "indoor",
                                          location %in% c(18, 31, 32) ~ "outdoor",
                                          location %in% c(30, 33:49, 90) ~ "transport"))
  
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
