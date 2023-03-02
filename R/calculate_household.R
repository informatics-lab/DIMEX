#' A function to calculate the house-hold not home air pollution
#'
#' @param act_dat - Dataset containing the act profiles
#'                  with associated ambient air pollution
#' @param pop_dat - Population dataset with attributes
#'                  needed for the mass balance equations
#' @param ambient - column containing the ambient air
#'                  pollution estimates
#' @param outvar - Column to output the household concent-
#'                 rations to
#' @return Dataset containing the sampled PM2.5 estimate
#'         for household.
#' @export
calculate_household <- function(act_dat, 
                             pop_dat, 
                             ambient,
                             outvar){
  # Preparing data for Mass balance equations
  act_dat <- act_dat %>%
    dplyr::arrange(pop_id, date, time) %>%
    dplyr::group_by(pop_id) %>%
    dplyr::mutate(uniid = 1:dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(date, time, pop_id)  %>%
    dplyr::left_join(pop_dat %>%
                dplyr::select(pop_id, housetype),
              by = 'pop_id') %>%
	as.data.frame()
  
  # Getting number of population 
  Npop <- length(unique(act_dat$pop_id))
  
  # Empty column 
  act_dat[,outvar] <- NA
  
  # Getting initial level of hou
  act_dat[which(act_dat$uniid == 1), outvar] <- rnorm(n = Npop, mean = 12, sd = 2)

  # Loop for each time point 
  for (i in 2:max(act_dat$uniid)){
    # Getting time point and previous time point
    tmp1 <- subset(act_dat, uniid == i)

    # penetration factor (Özkaynak et al. (1996))
    Fp <- rnorm(n = Npop, mean = 1, sd = 0.055)
    
    # deposition rate (Özkaynak et al. (1996))
    Fd <- rnorm(n = Npop, mean = 0.39, sd = 0.0825)
    
    # emission generating source (Özkaynak et al. (1996))
    SCooking <- rnorm(n = Npop, mean = 1.7, sd = 0.325)
    SOther <- rnorm(n = Npop, mean = 1.1, sd = 0.525)
    
    # Getting total emissions 
    # assumption: non-smokers
    S <- 
      # First adding the cooking emissions if they are cooking at home
      ((tmp1$activity %in% c(3110, 3100, 4210, 3190) & 
          tmp1$micro_group == 'home') + 0) * SCooking + 
      # Add in other emissions
      SOther
    
    # calculate air exchange rate (Murray and Burmaster (1995) Region 3)
    v <- if (unique(tmp1$season) == 1) {  # Winter
      rlnorm(n = Npop, meanlog = -0.958, sdlog = 0.589)
    } else if (unique(tmp1$season) == 2) {  # Spring
      rlnorm(n = Npop, meanlog = -0.802, sdlog = 0.782)
    } else if (unique(tmp1$season) == 3) {  # Summer
      rlnorm(n = Npop, meanlog = -0.588, sdlog = 0.612)
    } else if (unique(tmp1$season) == 4) {  # Autumn
      rlnorm(n = Npop, meanlog = -0.787, sdlog = 0.453)
    }
    
    # calculate volume of home (zoopla + onaverage.co.uk)
    V <- ((tmp1$housetype == 'detached') + 0) * EnvStats::rtri(Npop, min = 81, max = 214, mode = 159) * runif(1, min = 2.1, max = 2.6) + 
      ((tmp1$housetype == 'semi-detached') + 0) * EnvStats::rtri(Npop, min = 56, max = 204, mode = 84)  * runif(1, min = 2.1, max = 2.6) + 
      ((tmp1$housetype == 'terrace') + 0) * EnvStats::rtri(Npop, min = 33, max = 155, mode = 59)  * runif(1, min = 2.1, max = 2.6) + 
      ((tmp1$housetype == 'flat') + 0) * EnvStats::rtri(Npop, min = 34, max = 106, mode = 41)  * runif(1, min = 2.1, max = 2.6)
    
    # extract ambient concentration
    Cout <- tmp1[, ambient, drop = TRUE]
    
    # concentration added
    Cadd <-  S / V + v * Fp * Cout
    
    # Getting previous concentrations 
    Cbefore <- act_dat[which(act_dat$uniid == i-1), outvar, drop = TRUE]
    
    # calculate indoor concentration (Zidek et al. (2007))
    # Cbefore * (1 - v - Fd) + Cadd
    act_dat[which(act_dat$uniid == i), outvar] <- 
      (Cadd / (v + Fd)) + (Cbefore - (Cadd / (v + Fd))) * exp(-(v + Fd) * 1)
  }
  # Returning 
  return(act_dat %>% dplyr::select(-c(housetype)))
}
