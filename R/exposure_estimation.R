#' Estimate personal exposure for a specific MSOA.
#'
#' @param  activities_complete - sampled activities from earlier processing steps
#' @return  dataframe of personal exposure estimates
#'
#' @export
exposure_estimation <- function(activities_complete)
  activities_complete <- activities_complete %>%
    # Only keeping Q1 2021
    filter(date >= as.Date("2021-01-01")) %>%
    # Getting exposures 
    mutate(exposure_cams = ifelse(micro_group == "outdoor", pm25_cams_agg,
                                         ifelse(micro_group == "indoor", pm25_cams_agg_inh,
                                                ifelse(micro_group == "transport", pm25_cams_agg_tns,
                                                       ifelse(micro_group == "home", pm25_cams_agg_hhd, NA)))),
                  exposure_emep = ifelse(micro_group == "outdoor", pm25_emep_agg,
                                         ifelse(micro_group == "indoor", pm25_emep_agg_inh,
                                                ifelse(micro_group == "transport", pm25_emep_agg_tns,
                                                       ifelse(micro_group == "home", pm25_emep_agg_hhd, NA)))),
                  exposure_five = ifelse(micro_group == "outdoor", pm25_five,
                                         ifelse(micro_group == "indoor", pm25_five_inh,
                                                ifelse(micro_group == "transport", pm25_five_tns,
                                                       ifelse(micro_group == "home", pm25_five_hhd, NA)))))%>%
    # Averaging by day
    ddply(.(area_id, pop_id, date, daytype, daytype_label, season, season_label,
            sex, sex_label, agegr4, agegr4_label, nssec5, nssec5_label),
          summarize,
          exposure_cams = mean(exposure_cams),
          exposure_emep = mean(exposure_emep),
          exposure_five = mean(exposure_five),
          pm25_cams_agg = mean(pm25_cams_agg),
          pm25_emep_agg = mean(pm25_emep_agg),
          pm25_five = mean(pm25_five))