#' actigraph.sleepr
#'
#' This package implements three standard algorithms for sleep detection from ActiGraph data: Sadeh, Cole-Kripke and Tudor-Locke.
#'
#' In addition to the help pages, see the README page on \href{https://github.com/dipetkov/actigraph.sleepr}{github} for examples.
#'
#' @name actigraph.sleepr
#' @docType package
#' @useDynLib actigraph.sleepr, .registration = TRUE
#' @import dplyr ggplot2
#' @importFrom assertthat assert_that has_name
#' @importFrom rlang .data quo_text
#' @importFrom tidyr gather spread unnest
#' @importFrom purrr map map2
#' @importFrom data.table rleid
#' @importFrom zoo na.locf na.trim na.spline na.fill
#' @importFrom RcppRoll roll_mean roll_sd roll_sum
#' @importFrom lubridate duration ymd_hms time_length is.POSIXct floor_date
#' @importFrom stringr str_replace
#' @importFrom stats na.omit
#' @importFrom Rcpp sourceCpp
NULL

globalVariables(".")
