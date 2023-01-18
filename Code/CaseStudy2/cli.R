library(here)
library(optparse)
library(magrittr)

cli <- function() {
  OptionParser(option_list = list(
    make_option(c("--step"),
                help = "DIMEX processing step"),
    make_option(c("--prefix"),
                default = "~/Dropbox/Github/SPFFinalReport",
                help = "top-level directory containing Code/ and Data/")
  )) %>%
  parse_args %>%
  validate_args
}

validate_args <- function(opts) {
  if (!file.exists(opts$prefix)) {
    stop(paste("directory", opts$prefix, "does not exist. See --prefix option."))
  }
  opts
}

# Main program
main <- function() {
  opts <- cli()

  # Working director
  setwd(opts$prefix)

  # Run particular step
  if (opts$step == "1a") {
    source(here("Code", "CaseStudy2", "1a_DataPrep_StudyRegion.R"))
  } else if (opts$step == "1b") {
    source(here("Code", "CaseStudy2", "1b_DataPrep_Population.R"))
  } else {
    stop(paste("unknown step", opts$step))
  }
  
}

main()