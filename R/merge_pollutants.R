#' Merge PM2.5 pollutant data from CAMS and EMEP models.
#'
#' @param pm25_cams - Dataset containing PM2.5 levels from CAMS
#' @param pm25_emep - Dataset containing PM2.5 levels from EMEP
#' @param start_date - Start date
#' @param end_date - End date
#' @return Dataset of merged PM2.5 pollutant levels
#' @export
merge_pollutants <- function(pm25_cams, pm25_emep, start_date, end_date) {
  pm25_ctm <- pm25_cams %>%
    dplyr::select(area_id, date, hour, pm25_cams_agg) %>%
    dplyr::left_join(pm25_emep %>%
                dplyr::select(area_id, date, hour, pm25_emep_agg = pm25_cams_agg),
              by = c("area_id", "date", "hour")) %>%
    dplyr::filter(as.Date(date) >= as.Date(start_date) &
            as.Date(date) <= as.Date(end_date)) %>%
    dplyr::mutate(pm25_emep_agg = ifelse(is.na(pm25_emep_agg), pm25_cams_agg, pm25_emep_agg),
                  pm25_five = 5,
                  date = as.Date(date))
  pm25_ctm
}
