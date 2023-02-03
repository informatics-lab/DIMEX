#' Estimating exposure
#'
#' @param activities_complete - output data from activity sampler
#' @param pop_dat - population data frame
#' @param pm25_ctm - aggregated dataframe made up of EMEP and CAMS data
#' @param num_date_start - date range start (numerical value e.g. 18616)
#' @param num_date_end - date range end (numerical value e.g. 18717)
#' @return data frame of sampled activities
#'
#' @export

estimate_exposures <- function(actvities_complete, pop_dat, pm25_ctm, num_date_start, num_date_end) {
  # Preparing data for exposure modelling
  activities_complete <- activities_complete %>%
    # Only keeping specific period
    # TODO: convert date index to datetime format
    filter(as.numeric(date) >= num_date_start &
             as.numeric(date) <= num_date_end) %>%
    # Adding on demographic variables
    dplyr::left_join(pop_dat %>%
                       dplyr::select(pop_id, area_id, sex, sex_label, agegr4, agegr4_label, nssec5, nssec5_label),
                     by = 'pop_id') %>%
    # Merging on pm data
    dplyr::left_join(pm25_ctm %>%
                       dplyr::select(area_id, date, hour, pm25_cams_agg, pm25_five, pm25_emep_agg),
                     by = c('area_id', 'date', 'hour')) %>% 
    as.data.frame()

  # list relevant column names
  ambient_list <- list("pm25_cams_agg", "pm25_emep_agg", "pm25_five")
  
  for (i in 1:3){
    
    # Transportation exposures
    activities_complete <- calculate_transport(activities_complete, ambient = ambient_list[i], outvar = paste(ambient_list[i], "_tns", sep = ''))
    
    # Indoor-not-home exposures
    activities_complete <- calculate_indoor(activities_complete, ambient = ambient_list[i], outvar = paste(ambient_list[i], "_inh", sep = ''))
    
    # Household exposures
    activities_complete <- calculate_household(act_dat = activities_complete, pop_dat = pop_dat, 
                                               ambient = ambient_list[i], outvar = paste(ambient_list[i], "_hhd", sep = ''))
  }
  
  activities_complete
}


