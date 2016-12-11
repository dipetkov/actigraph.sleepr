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
#' @import tidyr
#' @import lazyeval
#' @importFrom Rcpp sourceCpp
#' @importFrom zoo na.locf na.trim na.spline
#' @importFrom RcppRoll roll_mean roll_sd roll_sum
#' @importFrom lubridate duration ymd_hms
#' @importFrom stats setNames
NULL

globalVariables(c("ave_awakening", "awakenings", "axis1", "axis2", "axis3",
                  "datatimestamp", "efficiency", "end_timestamp", "filtered",
                  "fragmentation_index", "in_bed_timestamp", "latency",
                  "length", "magnitude", "movement_index", "nonzero_epochs",
                  "onset_timestamp", "out_bed_timestamp", "settingname",
                  "settingvalue", "sleep", "sleep_1min", "start_timestamp",
                  "state", "time_asleep", "time_awake", "time_in_bed", ".",
                  "timestamp", "total_counts", "sleep_fragmentation_index"))
