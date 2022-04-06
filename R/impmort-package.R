#' impmort: Import Free Access Data For Mortality Modelling.
#'
#' Import mortality data from more than 41 countries using HMD data or the
#' French regional mortality (FRD) data with 3 levels of granularity. Interface
#' is very intuitive you just have to register on HMD and get your username and
#' password.

#' @import purrr
#' @import dplyr
#'
#' @docType package
#' @name impmort-package
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
## and data.table
if (getRversion() >= "4.1")  {
  gv <- c(".", ":=", "Country", "Total", "Age", "Gender", "Region", "Year", "Period")
  utils::globalVariables(gv)
}

# library(devtools)

# License
# use_gpl3_license()

# # Packages
# use_package("dplyr")
# use_package("tidyr")
# use_package("tibble")
# use_package("purrr")
# use_package("forcats")
# use_package("RCurl")
# use_package("rvest")

# # Tests----
# use_test("HMD")

# # Data----
# use_data_raw()

# # Patchnotes----
# use_news_md()
