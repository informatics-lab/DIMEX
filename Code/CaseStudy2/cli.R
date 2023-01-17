library(optparse)
library(magrittr)

cli <- function() {
  OptionParser(option_list = list(
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
}
