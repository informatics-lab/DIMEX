library(optparse)

cli <- function() {
  opts <- parse_args(OptionParser(option_list = list(
    make_option(c("--prefix"),
                default = "~/Dropbox/Github/SPFFinalReport",
                help = "top-level directory containing Code/ and Data/")
  )))
  if (!file.exists(opts$prefix)) {
    stop(paste("directory", opts$prefix, "does not exist. See --prefix option."))
  }
  opts
}
