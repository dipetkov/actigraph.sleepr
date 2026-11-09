#' actigraph.sleepr
#'
#' This package implements three standard algorithms for sleep
#'  detection from ActiGraph data: Sadeh, Cole-Kripke and Tudor-Locke.
#'
#' In addition to the help pages, see the
#' [README](https://github.com/dipetkov/actigraph.sleepr)
#' for examples.
#'
#' @name actigraph.sleepr
#' @useDynLib actigraph.sleepr, .registration = TRUE
#' @import dplyr
#' @importFrom assertthat assert_that has_name noNA
#' @importFrom rlang .data is_scalar_integerish
#' @importFrom Rcpp evalCpp
#' @importFrom lubridate duration ymd_hms time_length is.POSIXct floor_date
"_PACKAGE"

globalVariables(c(".", "timestamp", "where"))
