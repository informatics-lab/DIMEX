#' Estimate daily average personal exposure for a specific MSOA.
#'
#' @param  activities_complete - sampled activities from earlier processing steps
#' @return  dataframe of personal daily average exposure estimates
#'
#' @export
daily_averages <- function(
    activities_complete,
    start_date = "2021-01-01",
    end_date = "2021-03-31")
{
  activities_complete <- activities_complete %>%
    # Only keeping Q1 2021
    date_filter(start_date, end_date) %>%
    # Getting exposures 
    dplyr::mutate(exposure_cams = ifelse(micro_group == "outdoor", pm25_cams_agg,
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
    plyr::ddply(plyr::.(area_id, pop_id, date, daytype, season, sex, agegr4, nssec5),
          plyr::summarize,
          exposure_cams = mean(exposure_cams),
          exposure_emep = mean(exposure_emep),
          exposure_five = mean(exposure_five),
          pm25_cams_agg = mean(pm25_cams_agg),
          pm25_emep_agg = mean(pm25_emep_agg),
          pm25_five = mean(pm25_five))

  activities_complete
}
