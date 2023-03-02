#' Estimates exposure for three groups (transport, indoor and household)
#'
#' @param activities_complete - output dataframe from activity sampler
#' @param pop_dat - population dataframe
#' @param pm25_ctm - merged dataframe made up of EMEP and CAMS data
#' @return dataframe of completed activities
#'
#' @export

estimate_exposures <- function(
    activities_complete, pop_dat, pm25_ctm,
    start_date = "2020-12-20",
    end_date = "2021-03-31")
{
  # Preparing data for exposure modelling
  activities_complete <- activities_complete %>%
    # Only keeping specific period
    # TODO: convert date index to datetime format
    date_filter(start_date, end_date) %>%
    # Adding on demographic variables
    dplyr::left_join(pop_dat %>%
                       dplyr::select(pop_id, area_id, sex, agegr4, nssec5),
                     by = 'pop_id') %>%
    # Merging on pm data
    dplyr::left_join(pm25_ctm %>%
                       dplyr::select(area_id, date, hour, pm25_cams_agg, pm25_five, pm25_emep_agg),
                     by = c('area_id', 'date', 'hour')) %>% 
    as.data.frame()

  # Transportation exposures
  activities_complete <- calculate_transport(activities_complete, ambient = "pm25_cams_agg", outvar = "pm25_cams_agg_tns")
  activities_complete <- calculate_transport(activities_complete, ambient = "pm25_emep_agg", outvar = "pm25_emep_agg_tns")
  activities_complete <- calculate_transport(activities_complete, ambient = "pm25_five", outvar = "pm25_five_tns")
  
  # Indoor-not-home exposures
  activities_complete <- calculate_indoor(activities_complete, ambient = "pm25_cams_agg", outvar = "pm25_cams_agg_inh")
  activities_complete <- calculate_indoor(activities_complete, ambient = "pm25_emep_agg", outvar = "pm25_emep_agg_inh")
  activities_complete <- calculate_indoor(activities_complete, ambient = "pm25_five", outvar = "pm25_five_inh")
  
  # Household exposures
  activities_complete <- calculate_household(act_dat = activities_complete, pop_dat = pop_dat, 
                                             ambient = "pm25_cams_agg", outvar = "pm25_cams_agg_hhd")
  activities_complete <- calculate_household(act_dat = activities_complete, pop_dat = pop_dat, 
                                             ambient = "pm25_emep_agg", outvar = "pm25_emep_agg_hhd")
  activities_complete <- calculate_household(act_dat = activities_complete, pop_dat = pop_dat, 
                                             ambient = "pm25_five", outvar = "pm25_five_hhd")
  activities_complete
}
