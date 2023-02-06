#' A function to sample activities sequences from the time use survey
#'
#' @param  pop_dat - Population data to sample from
#' @param  tus_dat - Time use survey data to sample from
#' @param  nsample - Number of population samples
#' @param  weights - Sampling weights for the time use diaries
#' @param  pop_strata - Stratification variables for sampling
#'                      population data
#' @param  tus_strata - Stratification variables for sampling
#'                      time use data
#' @param  start_date - Start date for personal exposures
#' @param  end_date - End date for personal exposures
#' @param  keep - Variables to keep from the TUS
#' @return  Dataframe with sampled activity sequences for
#'          each individual between start_date end_date
#' @export
sample_population <- function(pop_dat,
                              tus_dat,
                              nsample,
                              weights = NULL,
                              pop_strata,
                              tus_strata,
                              start_date,
                              end_date,
                              keep) {
  ################
  ### Preamble ###
  ################
  # If no weights are provided for sampling then use equal weighting
  if (is.null(weights) == TRUE) {
    tus_dat$weights <- 1
  }
  # Else use the weights specified
  else {
    tus_dat$weights <- tus_dat[, weights]
  }
  ########################
  ### Getting metadata ###
  ########################
  # Getting a list of strata for activities
  stratification_labels <- tus_dat %>%
    stratify_by_column(tus_strata)
  # Getting activities ID and
  tus_act_id <- tus_dat %>%
    # Merging on stratification labels
    dplyr::left_join(stratification_labels,
      by = tus_strata
    )
  # Normalising the weights within each strata
  tus_act_id <- tus_act_id %>%
    # Merging on summary of weights in each strata
    dplyr::left_join(
      tus_act_id %>%
        dplyr::group_by(strata) %>%
        dplyr::summarise(sums = sum(weights)),
      by = "strata"
    ) %>%
    # Normalising weights
    dplyr::mutate(weights = weights / sums) %>%
    # Removing unnecessary columns
    dplyr::select(-c(sums))
  ##########################################
  ### Preparing population time profiles ###
  ##########################################
  # Sampling population to find exposures for
  pop_dat2 <- pop_dat %>%
    dplyr::group_by_at(.vars = pop_strata) %>%
    dplyr::sample_n(
      size = nsample,
      replace = FALSE
    )
  # Preparing shell dataset for sampling
  activities <- expand.grid(
    pop_id = pop_dat2$pop_id,
    date = seq(as.Date(start_date), as.Date(end_date), by = 1)
  ) %>%
    # Adding on day information
    dplyr::mutate(
      day_label = weekdays(date),
      day = day_number(date),
      daytype = day_type(day),
      daytype_label = day_type_label(day),
      season = season(lubridate::month(date)),
      season_label = season_label(lubridate::month(date))
    ) %>%
    # Merging on population data
    dplyr::left_join(
      pop_dat %>%
        dplyr::select_at(c("pop_id", "sex", "agegr4", "nssec5")),
      by = "pop_id"
    ) %>%
    # Merging on stratification labels
    dplyr::left_join(stratification_labels,
      by = tus_strata
    )
  # Removing unecessary datasets
  rm(pop_dat2, stratification_labels)
  ###################################
  ### Sampling activity sequences ###
  ###################################
  activities <- sample_sequences(activities, tus_act_id)

  # Merging on the activity data
  activities <- merge(activities,
    tus_dat[, c("act_id", "time", "time_label", keep)],
    by = "act_id"
  ) %>%
    dplyr::arrange(pop_id, date, time) %>%
    dplyr::select(-c(sex, agegr4, nssec5, strata))
  # Returning activity samples
  return(activities)
}

# Sample activity sequences
#
# Use time use survey activities
#
sample_sequences <- function(activities, tus_act_id) {
  # Empty dataset
  activities$act_id <- as.numeric(NA)
  # Loop for each strata
  for (i in unique(activities$strata)) {
    # Sampling within each strata
    activities$act_id[which(activities$strata == i)] <-
      sample(
        x = sample_x(tus_act_id, i),
        size = length(activities$pop_id[which(activities$strata == i)]),
        prob = sample_probability(tus_act_id, i),
        replace = TRUE
      )
  }
  activities
}

# Select candidates related to time use survey activity strata
sample_x <- function(tus_act_id, i) {
  tus_act_id$act_id[which(tus_act_id$strata == i)]
}

# Select probabilities related to time use survey activity strata
sample_probability <- function(tus_act_id, i) {
  tus_act_id$weights[which(tus_act_id$strata == i)]
}

# Group by vars and assign integer to each group in a new column called strata
stratify_by_column <- function(frame, column_names) {
  frame %>%
    # Grouping by stratification variables
    dplyr::group_by_at(.vars = column_names) %>%
    # Summarising
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::ungroup() %>%
    # Adding label to each strata
    dplyr::mutate(strata = 1:dplyr::n()) %>%
    dplyr::select(-c(n))
}
