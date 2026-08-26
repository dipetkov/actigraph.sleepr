#' Apply the Oakley algorithm
#'
#' The Oakley sleep scoring algorithm classifies each epoch as asleep or awake
#' from wrist activity counts. It was developed for the Actiwatch activity
#' monitoring system.
#'
#' @param agdb A `tibble` of activity data. Its epoch length must be 15, 30,
#'   60, or 120 seconds.
#' @param threshold Numeric wake threshold, or `"automatic"`. An epoch is
#'   scored as awake when its weighted activity score is greater than the
#'   threshold. The default is 40.
#' @return A `tibble` of activity data. A new column `sleep` indicates whether
#'   each epoch is scored as asleep (`"S"`) or awake (`"W"`).
#' @details
#' Oakley's weighted activity score is the sum of activity counts in a window
#' centred on the epoch, multiplied by epoch-length-specific weights. For
#' 60-second data, the score at epoch `t` is
#' ```
#' 0.04 * count[t - 2] + 0.20 * count[t - 1] + count[t] +
#'   0.20 * count[t + 1] + 0.04 * count[t + 2]
#' ```
#' The method uses the `axis1` count directly. The weights for 15-, 30-, and
#' 120-second epochs are those specified in the Actiwatch software manual.
#' Counts outside the observed series are treated as zero when scoring the
#' first and last epochs.
#'
#' With `threshold = "automatic"`, the wake threshold is calculated separately
#' for each group as `0.88888 * sum(count) / mobile_time_minutes`. An epoch is
#' mobile when its count is at least the number of 15-second intervals in that
#' epoch. `mobile_time_minutes` is the number of mobile epochs multiplied by
#' the epoch length in minutes.
#' @references
#' Oakley NR. *Validation with Polysomnography of the Sleepwatch Sleep/Wake
#' Scoring Algorithm Used by the Actiwatch Activity Monitoring System*.
#' Technical Report. Mini-Mitter; 1997.
#'
#' Actiwatch Communication and Sleep Analysis Software instruction manual.
#' Respironics, Inc. Available at
#' <https://fccid.io/JIAAWR1/Users-Manual/USERS-MANUAL-1-920937>.
#'
#' The implementation and weights were cross-checked against the
#' [`pyActigraphy` Oakley documentation](https://ghammad.github.io/pyActigraphy/_autosummary/pyActigraphy.sleep.ScoringMixin.Oakley.html).
#' Its [source implementation](https://ghammad.github.io/pyActigraphy/_modules/pyActigraphy/sleep/scoring_base.html#ScoringMixin.Oakley)
#' was also consulted.
#' @seealso [apply_sadeh()], [apply_cole_kripke()]
#' @examples
#' library("dplyr")
#' data("gtxplus1day")
#'
#' gtxplus1day %>%
#'   collapse_epochs(60) %>%
#'   apply_oakley()
#' @export
apply_oakley <- function(agdb, threshold = 40) {
  check_args_oakley(agdb, threshold)

  attr(agdb, "sleep_algorithm") <- "Oakley"

  agdb %>% group_modify(
    ~ apply_oakley_(., get_epoch_length(.), threshold)
  )
}

check_args_oakley <- function(agdb, threshold) {
  epoch_length <- get_epoch_length(agdb)
  assert_that(
    epoch_length %in% c(15, 30, 60, 120),
    msg = "Oakley assumes 15, 30, 60, or 120 second epochs."
  )
  check_no_missing_timestamps(agdb)
  check_no_missing_counts(agdb, "axis1")
  assert_that(
    identical(threshold, "automatic") ||
      (is.numeric(threshold) && length(threshold) == 1L && !is.na(threshold)),
    msg = "`threshold` must be a single numeric value or `\"automatic\"`."
  )
}

oakley_weights <- function(epoch_length) {
  switch(as.character(epoch_length),
    "15" = c(rep(0.04, 4), rep(0.20, 4), 4, rep(0.20, 4), rep(0.04, 4)),
    "30" = c(0.04, 0.04, 0.20, 0.20, 2, 0.20, 0.20, 0.04, 0.04),
    "60" = c(0.04, 0.20, 1, 0.20, 0.04),
    "120" = c(0.12, 0.50, 0.12)
  )
}

oakley_automatic_threshold <- function(count, epoch_length) {
  mobile <- count >= epoch_length / 15
  mobile_time_minutes <- sum(mobile) * epoch_length / 60
  assert_that(
    mobile_time_minutes > 0,
    msg = "Cannot calculate the automatic Oakley threshold: no mobile epochs."
  )
  0.88888 * sum(count) / mobile_time_minutes
}

apply_oakley_ <- function(data, epoch_length, threshold) {
  count <- data$axis1
  if (identical(threshold, "automatic")) {
    threshold <- oakley_automatic_threshold(count, epoch_length)
  }
  weights <- oakley_weights(epoch_length)
  half_window <- (length(weights) - 1) / 2
  padded_count <- c(rep(0, half_window), count, rep(0, half_window))
  score <- vapply(
    seq_along(count),
    function(i) sum(weights * padded_count[i:(i + 2 * half_window)]),
    numeric(1)
  )

  data %>% mutate(sleep = if_else(score <= threshold, "S", "W"))
}
