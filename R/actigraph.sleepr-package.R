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
#' @importFrom data.table rleid
#' @importFrom zoo na.locf na.trim na.spline na.fill
#' @importFrom RcppRoll roll_mean roll_sd roll_sum
#' @importFrom lubridate duration ymd_hms time_length
#' @importFrom stats setNames
#' @importFrom Rcpp sourceCpp
NULL

globalVariables(c("ave_awakening", "awakenings", "axis1", "axis2", "axis3",
                  "datatimestamp", "dozings", "dozings_1min", "efficiency",
                  "end_timestamp", "filtered", "fragmentation_index",
                  "in_bed_timestamp", "latency", "length", "magnitude",
                  "movement_index", "nonzero_epochs", "onset_timestamp",
                  "out_bed_timestamp", "period_id", "rev_id", "settingname",
                  "settingvalue", "sleep", "sleep_fragmentation_index",
                  "start_timestamp", "time_asleep", "time_awake",
                  "time_in_bed", "timestamp", "total_counts", "wear", "."))
