#' Apply the Troiano algorithm
#'
#' The Troiano algorithm detects periods of non-wear in activity data from an ActiGraph device. Such intervals are likely to represent invalid data and therefore should be excluded from downstream analysis. The algorithm formalizes a technique used to analyze the 2003-2004 NHANES data; the original SAS source code can be found at \url{http://riskfactor.cancer.gov/tools/nhanes_pam/}.
#' @param agdb A \code{tibble} (\code{tbl}) of activity data (at least) an \code{epochlength} attribute. The epoch length must be 60 seconds.
#' @param activity_threshold Highest activity level to be considered "zero"; an epoch with activity exceeding the threshold is considered a "spike". The default threshold is 0.
#' @param min_period_len Minimum number of consecutive "zero" epoch to start a non-wear period. The default is 60.
#' @param max_nonzero_count Epochs with activity greater than \code{max_nonzero_count} are labeled as "zero". The default is Inf.
#' @param spike_tolerance Also known as artifactual movement interval. At most \code{spike_tolerance} "nonzero" epochs can occur in sequence during a non-wear period without interrupting it. The default is 2.
#' @param spike_stoplevel An activity spike that exceeds \code{spike_stoplevel} counts ends a non-wear period, even if the spike tolerance has not been reached. The default is 100.
#' @param use_magnitude Logical. If true, the magnitude of the vector (axis1, axis2, axis3) is used to measure activity; otherwise the axis1 value is used. The default is FALSE.
#' @param endat_nnz_seq Logical. If true, a non-wear period ends with a run of nonzero epochs that is longer than \code{spike_tolerance}. The default is TRUE.
#' @details
#' The Troiano algorithm specifies that a non-wear period starts with \code{min_period_len} consecutive epochs/minutes of "zero" activity and ends with more than \code{spike_tolerance} epochs/minutes of "nonzero" activity.
#'
#' This implementation of the algorithm expects that the epochs are 60 second long.
#' @return A summary \code{tibble} of the detected non-wear periods. If the activity data is grouped, then non-wear periods are detected separately for each group.
#' @references RP Troiano, D Berrigan, KW Dodd, LC Mâsse, T Tilert and M McDowell. Physical activity in the united states measured by accelerometer. \emph{Medicine & Science in Sports & Exercise}, 40(1):181–188, 2008.
#' @references ActiLife 6 User's Manual by the ActiGraph Software Department. 04/03/2012.
#' @seealso \code{\link{apply_choi}}, \code{\link{collapse_epochs}}
#' @examples
#' file <- system.file("extdata", "GT3XPlus-RawData-Day01-10sec.agd",
#'                     package = "actigraph.sleepr")
#' agdb_10s <- read_agd(file)
#' agdb_60s <- collapse_epochs(agdb_10s, 60)
#' agdb_60s_nonwear <- apply_troiano(agdb_60s)
#' @export

apply_troiano <- function(agdb,
                          activity_threshold = 0,
                          min_period_len = 60,
                          max_nonzero_count = Inf,
                          spike_tolerance = 2,
                          spike_stoplevel = 100,
                          use_magnitude = FALSE,
                          endat_nnz_seq = TRUE) {

  check_args_nonwear_periods(agdb, "Troiano", use_magnitude)

  if (endat_nnz_seq)
    nonwear <- agdb %>%
      do(apply_troiano_seq_(., activity_threshold,
                            min_period_len, max_nonzero_count,
                            spike_tolerance,
                            spike_stoplevel,
                            use_magnitude))
  else
    nonwear <- agdb %>%
      do(apply_troiano_nonseq_(., activity_threshold,
                               min_period_len, max_nonzero_count,
                               spike_tolerance,
                               spike_stoplevel,
                               use_magnitude))

  attr(nonwear, "nonwear_algorithm") <- "Troiano"
  attr(nonwear, "min_period_len") <- min_period_len
  attr(nonwear, "max_nonzero_count") <- max_nonzero_count
  attr(nonwear, "spike_tolerance") <- spike_tolerance
  attr(nonwear, "spike_stoplevel") <- spike_stoplevel
  attr(nonwear, "activity_threshold") <- activity_threshold
  attr(nonwear, "use_magnitude") <- use_magnitude
  attr(nonwear, "endat_nnz_seq") <- endat_nnz_seq

  structure(nonwear, class = c("tbl_period", "tbl_df", "tbl", "data.frame"))
}

apply_troiano_seq_ <- function(data,
                               activity_threshold,
                               min_period_len, max_nonzero_count,
                               spike_tolerance,
                               spike_stoplevel,
                               use_magnitude) {
  data %>%
    mutate(magnitude = sqrt(axis1 ^ 2 + axis2 ^ 2 + axis3 ^ 2),
           count = if (use_magnitude) magnitude else axis1,
           state = ifelse(count <= activity_threshold |
                            count > max_nonzero_count, "N", "W"),
           state = ifelse(count > spike_stoplevel, "S", state)) %>%
    group_by(rleid = rleid(state)) %>%
    summarise(state = first(state),
              timestamp = first(timestamp),
              length = n()) %>%
    mutate(state = ifelse(state == "W" &
                            lead(state, default = "") == "N" &
                            length <= spike_tolerance,
                          NA, state),
           # Since `na.locf` can't impute leading NAs, fill in those with "W"
           state = ifelse(row_number() == 1 & is.na(state), "W", state),
           # Fill in NAs with the most recent zero/nonzero state
           state = na.locf(state)) %>%
    group_by(rleid = rleid(state)) %>%
    summarise(state = first(state),
              timestamp = first(timestamp),
              length = sum(length)) %>%
    filter(state == "N",
           length >= min_period_len) %>%
    mutate(end_timestamp = timestamp + duration(length, units = "mins")) %>%
    rename(start_timestamp = timestamp) %>%
    select(start_timestamp, end_timestamp, length)
}

apply_troiano_nonseq_ <- function(data,
                                  activity_threshold,
                                  min_period_len, max_nonzero_count,
                                  spike_tolerance,
                                  spike_stoplevel,
                                  use_magnitude) {
  x <- data %>%
    mutate(magnitude = sqrt(axis1 ^ 2 + axis2 ^ 2 + axis3 ^ 2),
           count = if (use_magnitude) magnitude else axis1,
           count = ifelse(count > max_nonzero_count, 0, count),
           length = wle(count, activity_threshold,
                        spike_tolerance, spike_stoplevel)) %>%
    # Don't combine these filter conditions in one statement as
    # filter(condition1, condition2) simply filters out *all* rows
    filter(length >= min_period_len) %>%
    filter(timestamp - lag(timestamp, default = 0) > 1) %>%
    mutate(end_timestamp = timestamp + duration(length, units = "mins")) %>%
    rename(start_timestamp = timestamp) %>%
    select(start_timestamp, end_timestamp, length)
  # Create empty data frame with the same column specification as x
  y <- x %>% filter(row_number() < 1)
  # Remove periods which overlap with previous periods
  while (nrow(x)) {
    z <- x %>% filter(row_number() == 1)
    x <- x %>% filter(start_timestamp > z$start_timestamp +
                        duration(z$length, units = "mins"))
    y <- bind_rows(y, z)
  }
  y
}
