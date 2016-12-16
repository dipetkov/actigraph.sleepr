#' Apply the Tudor-Locke algorithm
#'
#' The Tudor-Locke algorithm detects periods of time in bed and, for each period, computes sleep quality metrics such as total minutes in bed, total sleep time, number and average length of awakenings, movement and fragmentation index.
#' @param agdb A \code{tibble} (\code{tbl}) of activity data (at least) an \code{epochlength} attribute. The epoch length must be 60 seconds. Each epoch should be scored as asleep (S) or awake (W), using the Sadeh, the Cole-Kripke or a custom algorithm.
#' @param n_bedtime_start Bedtime definition, in minutes. The default is 5.
#' @param n_wake_time_end Wake time definition, in minutes. The default is 10.
#' @param min_sleep_period Min sleep period length, in minutes. The default is 160.
#' @param max_sleep_period Max sleep period length, in minutes. The default is 1440 (24 hours).
#' @param min_nonzero_epochs Min number of epochs with non-zero activity. The default is 0.
#' @return A summary \code{tibble} of the detected sleep periods. If the activity data is grouped, then sleep periods are detected separately for each group.
#' \describe{
#'   \item{in_bed_timestamp}{The first minute of the bedtime.}
#'   \item{out_bed_timestamp}{The first minute of wake time.}
#'   \item{onset_timestamp}{The first minute that the algorithm scores "asleep".}
#'   \item{latency}{The time elapsed between bedtime and sleep onset. By the definition of Tudor-Locke, latency is 0.}
#'   \item{total_counts}{The sum of activity counts for the entire sleep period.}
#'   \item{efficiency}{The number of sleep minutes divided by the bedtime  minutes.}
#'   \item{time_in_bed}{The duration of the sleep period, in minutes.}
#'   \item{time_asleep}{Total Sleep Time (TST) – The number of minutes scored as "asleep" during the sleep period.}
#'   \item{time_awake}{Wake after Sleep Onset (WASO) – The number of minutes scored as "awake", minus the sleep latency, during the sleep period.}
#'   \item{awakenings}{The number of awakening episodes.}
#'   \item{ave_awakening}{The average length, in minutes, of all awakening episodes.}
#'   \item{movement_index}{Proportion of awake time out of the total time in bed, in percentages.}
#'   \item{fragmentation_index}{Proportion of one-minute sleep bouts out of the number of sleep bouts of any length, in percentages.}
#'   \item{sleep_fragmentation_index}{Sleep fragmentation index – The sum of the movement and fragmentation indices.}
#' }
#' @details
#' Once each one-minute epoch is labeled as asleep (S) or awake (W), we can use the Tudor-Locke algorithm to detect periods of \emph{bedtime} and \emph{sleep time}. By definition, sleep time < bedtime since one can be in bed and not sleeping.
#'
#' Bedtime is (the first minute of) \code{n_bedtime_start} consecutive epochs/minutes labeled S. Similarly, wake time is (the first minute of) of \code{n_wake_time_end} consecutive epochs/minutes labeled W, after a period of sleep. The time block between bedtime and wake time is one sleep period, if the time elapsed is at least \code{min_sleep_period} minutes. There can be multiple sleep periods in 24 hours but a sleep period cannot be longer than \code{max_sleep_period} minutes.
#'
#' For each sleep period, the algorithm calculates several measures of sleep quality such as time asleep and time awake, number and average length of awakenings, and movement and fragmentation indices.
#' @references C Tudor-Locke, TV Barreira, JM Schuna Jr, EF Mire and PT Katzmarzyk. Fully automated waist-worn accelerometer algorithm for detecting children's sleep-period time separate from 24-h physical activity or sedentary behaviors. \emph{Applied Physiology}, Nutrition, and Metabolism, 39(1):53–57, 2014.
#' @references ActiLife 6 User's Manual by the ActiGraph Software Department. 04/03/2012.
#' @seealso \code{\link{apply_sadeh}}, \code{\link{apply_cole_kripke}}
#' @examples
#' file <- system.file("extdata", "GT3XPlus-RawData-Day01-10sec.agd",
#'                     package = "actigraph.sleepr")
#'
#' library("lubridate")
#' library("dplyr")
#'
#' agdb_10s <- read_agd(file)
#' agdb_60s <- collapse_epochs(agdb_10s, 60) %>%
#'   filter(day(timestamp) == 28)
#'
#' # Detect sleep periods using Sadeh as the sleep/awake algorithm
#' # and Tudor-Locke as the sleep period algorithm
#' agdb_60s_scored <- apply_sadeh(agdb_60s)
#' agdb_60s_sleep <- apply_tudor_locke(agdb_60s_scored, min_sleep_period = 60)
#' agdb_60s_sleep
#'
#' # Group and summarize by an extra varible: hour < 6 or not
#' # This grouping is chosen not because it is interesting but to split
#' # one longer sleep period in two, for illustration of how grouping works
#' agdb_60s <- agdb_60s %>%
#'   mutate(hour_lt6 = hour(timestamp) < 6) %>%
#'   group_by(hour_lt6)
#' agdb_60s_scored <- apply_sadeh(agdb_60s)
#' agdb_60s_sleep <- apply_tudor_locke(agdb_60s_scored, min_sleep_period = 60)
#' agdb_60s_sleep
#' @export

apply_tudor_locke <- function(agdb,
                              n_bedtime_start = 5,
                              n_wake_time_end = 10,
                              min_sleep_period = 160,
                              max_sleep_period = 1440,
                              min_nonzero_epochs = 0) {

  check_args_sleep_periods(agdb, "Tudor-Locke")

  # TODO: Some parameter combinations might not make sense.
  # For example, I expect that:
  # * min_nonzero_epochs < min_sleep_period
  # * n_bedtime_start + n_wake_time_end < min_sleep_period

  sleep <- agdb %>%
    do(apply_tudor_locke_(., n_bedtime_start, n_wake_time_end,
                          min_sleep_period, max_sleep_period,
                          min_nonzero_epochs))

  attr(sleep, "sleep_algorithm") <- attr(agdb, "sleep_algorithm")
  attr(sleep, "period_algorithm") <- "Tudor-Locke"
  attr(sleep, "n_bedtime_start") <- n_bedtime_start
  attr(sleep, "n_wake_time_end") <- n_wake_time_end
  attr(sleep, "min_sleep_period") <- min_sleep_period
  attr(sleep, "max_sleep_period") <- max_sleep_period
  attr(sleep, "min_nonzero_epochs") <- min_nonzero_epochs

  structure(sleep, class = c("tbl_period", "tbl_df", "tbl", "data.frame"))
}

apply_tudor_locke_ <- function(data,
                               n_bedtime_start, n_wake_time_end,
                               min_sleep_period, max_sleep_period,
                               min_nonzero_epochs) {

  # It seems that a sleep period must be followed by a sequence of n W's
  # where n = n_wake_time_end, *even at the end of the time series*.
  # This means that ActiLife filters out a sleep period, which ends when
  # the time series ends. On the other hand, a sleep period is allowed
  # to start when the time series starts. Makes sense?
  end_time <- last(data$timestamp) -
    duration(n_wake_time_end, units = "min")

  # The computation is not hard but the code looks intimidating,
  # so I'll summarize the logic here.
  #
  # There are two "rounds" of `group_by`, `summarise`, `mutate` operations.
  #
  # The `group_by` detects uninteruppted runs of asleep/awake epochs.
  # For example:
  # state is c( W,  W,  S,  W,  W,  W,  S,  S,  S,  W,  S,  W)
  # group is c( 1,  1,  2,  3,  3,  3,  4,  4,  4,  5,  6,  7)
  #
  # The `summarise` computes a few statistics necessary to compute the final
  # sleep quality metrics that we are interested in.
  #
  # Some runs of asleep/awake epochs might be too short. The `mutate`
  # sets these to NA so that we can combine them into longer runs
  # in the next round of `group_by`, `summarise`, `mutate`.
  #
  # For example, suppose that runs of just one epoch are too short.
  # state is c( W,  W, NA,  W,  W,  W,  S,  S,  S, NA, NA, NA)
  # Fill in the NAs with the most recent non-NA state.
  # state is c( W,  W,  W,  W,  W,  W,  S,  S,  S,  S,  S,  S)
  # group is c( 1,  1,  1,  1,  1,  1,  2,  2,  2,  2,  2,  2)
  #
  # Note that we don't always end up flipping short S's to W's and
  # short W's to S's. That's why we first set short runs to NA and
  # then impute them with na.locf.
  #
  # There are also a bunch of book-keeping `mutate`s to calculate
  # several measures of sleep quality for each sleep period.
  # This `mutate`s don't involve the `state`.

  data %>%
    # First round of `group_by`, `summarise`, `mutate` operations
    # Return the stop/end indices for runs of repeated value
    group_by(rleid = rleid(state)) %>%
    summarise(timestamp = first(timestamp),
              state = first(state),
              time_in_bed = n(),                 # number of epochs
              nonzero_epochs = sum(axis1 > 0), # number of epochs with activity
              total_counts = sum(axis1)) %>%   # total activity
    mutate(awakenings = (state == "W"),
           sleep = (state == "S"),
           sleep_1min = (state == "S" & time_in_bed == 1),
           time_asleep = if_else(state == "W" & time_in_bed < n_wake_time_end,
                                 0L, time_in_bed),
           # Set the state of short runs to NA
           state = if_else( (state == "W" & time_in_bed < n_wake_time_end) |
                              (state == "S" & time_in_bed < n_bedtime_start),
                            NA_character_, state),
           # A special case that I am not sure how to handle:
           # Leading NAs can't be filled in with `na.locf`.
           # To be conservative, I will fill in such NAs with "W".
           state = if_else(row_number() == 1 & is.na(state), "W", state),
           # Fill in NAs with the most recent sleep/awake state
           # TODO: Check that `na.locf` works as expected
           # when `state` is a character vector
           state = na.locf(state)) %>%
    # Second round of `group_by`, `summarise`, `mutate` operations
    group_by(rleid = rleid(state)) %>%
    summarise(timestamp = first(timestamp),
              state = first(state),
              total_counts = sum(total_counts, na.rm = TRUE),
              time_in_bed = sum(time_in_bed),
              time_asleep = sum(time_asleep),
              nonzero_epochs = sum(nonzero_epochs),
              awakenings = sum(awakenings),
              sleep = sum(sleep),
              sleep_1min = sum(sleep_1min)) %>%
    mutate(fragmentation_index =
             if_else(sleep > 0, 100 * sleep_1min / sleep, 0),
           movement_index = 100 * nonzero_epochs / time_in_bed,
           # Set the state of short sleep runs to "W";
           # no need to set to NA and then fill in the NAs
           # as here we flip only "S" states (S -> W)
           state = if_else(state == "S" & time_in_bed < min_sleep_period,
                           "W", state)) %>%
    # Filter out wake (W) periods as well assleep periods that
    # fail the min_nonzero_epochs and max_sleep_period criteria
    filter(state == "S",
           # Deal with edge case to reproduce ActiLife results: end <= end_time
           nonzero_epochs >= min_nonzero_epochs,
           time_in_bed <= max_sleep_period) %>%
    # That's it. The rest are trivial manipulations to compute
    # various sleep quality metrics.
    mutate(out_bed_timestamp =
             timestamp + duration(time_in_bed, units = "mins"),
           sleep_fragmentation_index = movement_index + fragmentation_index,
           time_awake = time_in_bed - time_asleep,
           ave_awakening = if_else(awakenings > 0, time_awake / awakenings, 0),
           efficiency = 100 * time_asleep / time_in_bed,
           # latency is the difference between in bed and onset times
           onset_timestamp = timestamp, latency = 0) %>%
    rename(in_bed_timestamp = timestamp) %>%
    select(in_bed_timestamp, out_bed_timestamp, onset_timestamp, latency,
           total_counts, efficiency, time_in_bed, time_asleep, time_awake,
           awakenings, ave_awakening, movement_index, fragmentation_index,
           sleep_fragmentation_index) %>%
    mutate_each(funs(as.integer), latency, total_counts,
                time_in_bed, time_asleep, time_awake, awakenings)
}
