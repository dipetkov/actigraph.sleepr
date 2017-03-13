#' Apply the Tudor-Locke algorithm
#'
#' The Tudor-Locke algorithm detects periods of time in bed and, for each period, computes sleep quality metrics such as total minutes in bed, total sleep time, number and average length of awakenings, movement and fragmentation index.
#' @param agdb A \code{tibble} (\code{tbl}) of activity data (at least) an \code{epochlength} attribute. The epoch length must be 60 seconds. Each epoch should be scored as asleep (S) or awake (W), using the Sadeh, the Cole-Kripke or a custom algorithm.
#' @param n_bedtime_start Bedtime definition, in minutes. The default is 5.
#' @param n_wake_time_end Wake time definition, in minutes. The default is 10.
#' @param min_sleep_period Minimum sleep period length, in minutes. The default is 160.
#' @param max_sleep_period Maximum sleep period length, in minutes. The default is 1440 (24 hours).
#' @param min_nonzero_epochs Minimum number of epochs with non-zero activity. The default is 0.
#' @return A summary \code{tibble} of the detected sleep periods. If the activity data is grouped, then sleep periods are detected separately for each group.
#' \describe{
#'   \item{in_bed_time}{The first minute of the bedtime.}
#'   \item{out_bed_time}{The first minute of wake time.}
#'   \item{onset}{The first minute that the algorithm scores "asleep".}
#'   \item{latency}{The time elapsed between bedtime and sleep onset. By the definition of Tudor-Locke, latency is 0.}
#'   \item{efficiency}{The number of sleep minutes divided by the bedtime minutes.}
#'   \item{duration}{The duration of the sleep period, in minutes.}
#'   \item{activity_counts}{The sum of activity counts for the entire sleep period.}
#'   \item{total_sleep_time}{The number of minutes scored as "asleep" during the sleep period.}
#'   \item{wake_after_onset}{The number of minutes scored as "awake", minus the sleep latency, during the sleep period.}
#'   \item{nb_awakenings}{The number of awakening episodes.}
#'   \item{ave_awakening}{The average length, in minutes, of all awakening episodes.}
#'   \item{movement_index}{Proportion of awake time out of the total time in bed, in percentages.}
#'   \item{fragmentation_index}{Proportion of one-minute sleep bouts out of the number of sleep bouts of any length, in percentages.}
#'   \item{sleep_fragmentation_index}{The sum of the movement and fragmentation indices.}
#'   \item{nonzero_epochs}{The number of epochs with activity > 0 (nonzero epochs).}
#' }
#' @details
#' Once each one-minute epoch is labeled as asleep (S) or awake (W), we can use the Tudor-Locke algorithm to detect periods of \emph{bedtime} and \emph{sleep time}. By definition, sleep time < bedtime since one can be in bed and not sleeping.
#'
#' Bedtime is (the first minute of) \code{n_bedtime_start} consecutive epochs/minutes labeled asleep (S). Similarly, wake time is (the first minute of) of \code{n_wake_time_end} consecutive epochs/minutes labeled awake (W), after a period of sleep. The block of time between bedtime and wake time is one sleep period, if the time elapsed is at least \code{min_sleep_period} minutes. There can be multiple sleep periods in 24 hours but a sleep period cannot be longer than \code{max_sleep_period} minutes.
#'
#' For each sleep period, the algorithm calculates several measures of sleep quality such as time asleep and time awake, number and average length of awakenings, and movement and fragmentation indices.
#'
#' This implementation of the Tudor-Locke algorithm detects all the sleep periods that ActiLife detects and, in some cases, it detects \emph{additional} sleep periods. There are (at least) two such cases:
#' \enumerate{
#'   \item{ActiLife filters out some sleep periods with \emph{exactly} \code{min_nonzero_epochs} nonzero epochs. Alternatively, ActiLife computes the number of nonzero epochs in a sleep period differently and sometimes underestimates \code{nonzero_epochs} compared to \code{apply_tudor_locke}.}
#'   \item{ActiLife filters out sleep periods that end when the activity data ends, i.e., when the \code{out_bed_time} is also the final timestamp in the \code{agdb} table.}
#' }
#' @references C Tudor-Locke, TV Barreira, JM Schuna Jr, EF Mire and PT Katzmarzyk. Fully automated waist-worn accelerometer algorithm for detecting children's sleep-period time separate from 24-h physical activity or sedentary behaviors. \emph{Applied Physiology}, Nutrition, and Metabolism, 39(1):53–57, 2014.
#' @references ActiLife 6 User's Manual by the ActiGraph Software Department. 04/03/2012.
#' @seealso \code{\link{apply_sadeh}}, \code{\link{apply_cole_kripke}}
#' @examples
#' library("dplyr")
#' library("lubridate")
#' data("gtxplus1day")
#'
#' # Detect sleep periods using Sadeh as the sleep/awake algorithm
#' # and Tudor-Locke as the sleep period algorithm
#' agdb <- gtxplus1day %>%
#'   collapse_epochs(60) %>%
#'   filter(day(timestamp) == 28)
#' periods_sleep <- agdb %>%
#'   apply_sadeh() %>%
#'   apply_tudor_locke(min_sleep_period = 60)
#' periods_sleep
#'
#' # Group and summarize by an extra varible (hour < 6 or not), which
#' # splits one long sleep period in two
#' agdb <- agdb %>%
#'   mutate(hour_lt6 = hour(timestamp) < 6) %>%
#'   group_by(hour_lt6)
#' periods_sleep <- agdb %>%
#'   apply_sadeh() %>%
#'   apply_tudor_locke(min_sleep_period = 60)
#' periods_sleep
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
  sleep <-
    structure(sleep,
              class = c("tbl_period", "tbl_df", "tbl", "data.frame"),
              sleep_algorithm = attr(agdb, "sleep_algorithm"),
              period_algorithm = "Tudor-Locke",
              n_bedtime_start = n_bedtime_start,
              n_wake_time_end = n_wake_time_end,
              min_sleep_period = min_sleep_period,
              max_sleep_period = max_sleep_period,
              min_nonzero_epochs = min_nonzero_epochs)

  if (is.grouped_df(agdb))
    sleep <- sleep %>% group_by_(as.character(groups(agdb)))

  sleep
}

apply_tudor_locke_ <- function(data,
                               n_bedtime_start, n_wake_time_end,
                               min_sleep_period, max_sleep_period,
                               min_nonzero_epochs) {

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

  data %>%
    # First round of `group_by`, `summarise`, `mutate` operations
    # Return the stop/end indices for runs of repeated value
    group_by(rleid = rleid(sleep)) %>%
    summarise(timestamp = first(timestamp),
              sleep = first(sleep),
              duration = n(),                 # number of epochs
              nonzero_epochs = sum(axis1 > 0), # number of epochs with activity
              activity_counts = sum(axis1)) %>% # total activity
    mutate(nb_awakenings = (sleep == "W"),
           dozings = (sleep == "S"),
           dozings_1min = (sleep == "S" & duration == 1),
           total_sleep_time = if_else(sleep == "W" & duration < n_wake_time_end,
                                      0L, duration),
           # Set the state of short runs to NA
           sleep = if_else( (sleep == "W" & duration < n_wake_time_end) |
                              (sleep == "S" & duration < n_bedtime_start),
                            NA_character_, sleep),
           # A special case that I am not sure how to handle:
           # Leading NAs can't be filled in with `na.locf`.
           # To be conservative, I will fill in such NAs with W (awake).
           sleep = if_else(row_number() == 1 & is.na(sleep), "W", sleep),
           # Fill in NAs with the most recent sleep/awake state
           sleep = na.locf(sleep)) %>%
    # Second round of `group_by`, `summarise`, `mutate` operations
    group_by(rleid = rleid(sleep)) %>%
    summarise(timestamp = first(timestamp),
              sleep = first(sleep),
              activity_counts = sum(activity_counts, na.rm = TRUE),
              duration = sum(duration),
              total_sleep_time = sum(total_sleep_time),
              nonzero_epochs = sum(nonzero_epochs),
              nb_awakenings = sum(nb_awakenings),
              dozings = sum(dozings),
              dozings_1min = sum(dozings_1min)) %>%
    mutate(fragmentation_index =
             if_else(dozings > 0, 100 * dozings_1min / dozings, 0),
           movement_index = 100 * nonzero_epochs / duration,
           # Set the state of short sleep runs to awake;
           # no need to set to NA and then fill in the NAs
           # as here we flip only asleep states to awake
           sleep = if_else(sleep == "S" & duration < min_sleep_period,
                           "W", sleep)) %>%
    # Filter out wake (W) periods as well as sleep periods that
    # fail the min_nonzero_epochs and max_sleep_period criteria
    filter(sleep == "S",
           nonzero_epochs >= min_nonzero_epochs,
           duration <= max_sleep_period) %>%
    # That's it. The rest are trivial manipulations to compute
    # various sleep quality metrics.
    mutate(out_bed_time =
             timestamp + duration(duration, "mins"),
           sleep_fragmentation_index = movement_index + fragmentation_index,
           wake_after_onset = duration - total_sleep_time,
           ave_awakening =
             if_else(nb_awakenings > 0, wake_after_onset / nb_awakenings, 0),
           efficiency = 100 * total_sleep_time / duration,
           # latency is the difference between in bed and onset times
           onset = timestamp, latency = 0) %>%
    rename(in_bed_time = timestamp) %>%
    select(in_bed_time, out_bed_time, onset, latency, efficiency,
           duration, activity_counts, nonzero_epochs, total_sleep_time,
           wake_after_onset, nb_awakenings, ave_awakening, movement_index,
           fragmentation_index, sleep_fragmentation_index) %>%
    mutate_at(vars(latency, activity_counts, nonzero_epochs, duration,
                   total_sleep_time, wake_after_onset, nb_awakenings),
              as.integer)
}
