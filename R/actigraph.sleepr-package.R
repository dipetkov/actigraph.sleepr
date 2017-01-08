#' actigraph.sleepr
#'
#' This package implements three standard algorithms for sleep detection from ActiGraph data: Sadeh, Cole-Kripke and Tudor-Locke.
#'
#' In addition to the help pages, see the README page on \href{https://github.com/dipetkov/actigraph.sleepr}{github} for examples.
#'
#' @name actigraph.sleepr
#' @useDynLib actigraph.sleepr
#' @docType package
#' @import dplyr
#' @importFrom tidyr gather spread unnest
#' @importFrom lazyeval interp
#' @importFrom purrr map map2
#' @importFrom data.table rleid
#' @importFrom zoo na.locf na.trim na.spline na.fill
#' @importFrom RcppRoll roll_mean roll_sd roll_sum
#' @importFrom lubridate duration ymd_hms time_length
#' @importFrom stats setNames
#' @importFrom Rcpp sourceCpp
NULL

globalVariables(c( ".", "activity_counts", "ave_awakening", "axis1", "axis2",
                   "axis3", "datatimestamp", "dozings", "dozings_1min",
                   "duration", "efficiency", "end_timestamp", "filtered",
                   "fragmentation_index", "in_bed_time", "latency", "length",
                   "magnitude", "movement_index", "nb_awakenings",
                   "nonzero_epochs", "onset", "out_bed_time", "period_id",
                   "rev_id", "settingname", "settingvalue", "sleep",
                   "sleep_fragmentation_index", "start_timestamp", "timestamp",
                   "total_sleep_time", "wake_after_onset", "wear"))
