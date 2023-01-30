##############################################
### Filling in missings in the TUS dataset ###
##############################################
# Filling in missings using most popular activities
fill_in_missings <- function(tus_dat) {
  tus_dat <- tus_dat %>%
    # Setting missings to NA
    dplyr::mutate(location_popular = ifelse(location %in% c(-9, 0, 10, 99), NA, location)) %>%
    # Merging on most popular location for each activity by strata
    left_join(tus_dat %>%
                # Only keeping non-missing locations
                filter(!(location %in% c(-9, 0, 10, 99))) %>%
                # Summarising by stratum and activity
                dplyr::group_by_at(c('sex', 'agegr4', 'nssec5', 'daytype', 'activity', 'location')) %>%
                dplyr::summarise(n = length(location))  %>% 
                dplyr::group_by_at(c('sex', 'agegr4', 'nssec5', 'daytype', 'activity')) %>%
                # Getting proportion of time spent in each location by activity
                dplyr::mutate(p = n/sum(n),
                              tmp = max(p)) %>%
                # Only keeping most popular activity
                dplyr::filter(p == tmp) %>%
                # Keeping first if more than one selected
                dplyr::mutate(tmp = min(location)) %>%
                dplyr::filter(location == tmp) %>%
                # Ungrouping
                dplyr::ungroup() %>%
                # Only keeping relevant columns
                dplyr::select_at(c('sex', 'agegr4', 'nssec5', 'daytype', 'activity', 'location')) %>%
                # Renaming columns
                dplyr::rename(tmp = location),
              by = c('sex', 'agegr4', 'nssec5', 'daytype', 'activity'))  %>%
    # Filling in the missings
    dplyr::mutate(location_popular = ifelse(is.na(location_popular), 
                                            tmp,
                                            location_popular)) %>%
    # Removing uncessary columns 
    dplyr::select(-c(tmp))  %>%
    # Merging on most popular location for each activity by strata
    left_join(tus_dat %>%
                # Only keeping non-missing locations
                filter(!(location %in% c(-9, 0, 10, 99))) %>%
                # Summarising by stratum and activity
                dplyr::group_by_at(c('activity', 'location')) %>%
                dplyr::summarise(n = length(location))  %>% 
                dplyr::group_by_at(c('activity')) %>%
                # Getting proportion of time spent in each location by activity
                dplyr::mutate(p = n/sum(n),
                              tmp = max(p)) %>%
                # Only keeping most popular activity
                dplyr::filter(p == tmp) %>%
                # Keeping first if more than one selected
                dplyr::mutate(tmp = min(location)) %>%
                dplyr::filter(location == tmp) %>%
                # Ungrouping
                dplyr::ungroup() %>%
                # Only keeping relevant columns
                dplyr::select_at(c('activity', 'location')) %>%
                # Renaming columns
                dplyr::rename(tmp = location),
              by = c('activity'))  %>%
    # Filling in the missings
    dplyr::mutate(location_popular = ifelse(is.na(location_popular), 
                                            tmp,
                                            location_popular)) %>%
    # Removing uncessary columns 
    dplyr::select(-c(tmp))%>%
    # Merging on location labels
    left_join(read_csv("Data/Raw/TimeUseSurvey/uktus_metadata_location.csv") %>%
                dplyr::select(location_popular = location, 
                              location_popular_label = location_label),
              by = 'location_popular')
  tus_dat
}
