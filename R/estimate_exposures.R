#' Estimating exposure
#'
#' @param activities_complete - output data from activity sampler
#' @param pop_dat - population data frame
#' @param pm25_ctm - aggregated dataframe made up of EMEP and CAMS data
#' @return data frame of completed activities
#'
#' @export

estimate_exposures <- function(actvities_complete, pop_dat, pm25_ctm) {
  # Preparing data for exposure modelling
  activities_complete <- activities_complete %>%
    # Only keeping specific period
    # TODO: convert date index to datetime format
    filter(as.numeric(date) >= 18616 &
             as.numeric(date) <= 18717) %>%
    # Adding on demographic variables
    dplyr::left_join(pop_dat %>%
                       dplyr::select(pop_id, area_id, sex, sex_label, agegr4, agegr4_label, nssec5, nssec5_label),
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



