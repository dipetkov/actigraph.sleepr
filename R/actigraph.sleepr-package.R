#' actigraph.sleepr
#'
#' This package implements three standard algorithms for sleep detection from ActiGraph data: Sadeh, Cole-Kripke and Tudor-Locke.
#'
#' In addition to the help pages, see the README page on \href{https://github.com/dipetkov/actigraph.sleepr}{github} for examples.
#'
#' @name actigraph.sleepr
#' @useDynLib actigraph.sleepr
#' @docType package
#' @import dplyr ggplot2
#' @importFrom tidyr gather spread unnest
#' @importFrom lazyeval interp
#' @importFrom purrr map map2
#' @importFrom data.table rleid
#' @importFrom zoo na.locf na.trim na.spline na.fill
#' @importFrom RcppRoll roll_mean roll_sd roll_sum
#' @importFrom lubridate duration ymd_hms time_length is.POSIXct
#' @importFrom stats setNames na.omit
#' @importFrom Rcpp sourceCpp
NULL

globalVariables(c( ".", "a", "b", "activity_counts", "ave_awakening", "axis1",
                   "axis2", "axis3", "datatimestamp", "dozings", "dozings_1min",
                   "duration", "efficiency", "filtered", "fragmentation_index",
                   "in_bed_time", "key", "latency", "length", "magnitude",
                   "movement_index", "nb_awakenings", "nonzero_epochs",
                   "onset", "out_bed_time", "period_id", "period_end",
                   "period_start", "rev_id", "settingname", "settingvalue",
                   "sleep", "sleep_fragmentation_index", "timestamp", "tz",
                   "total_sleep_time", "wake_after_onset", "x", "y", "wear"))
