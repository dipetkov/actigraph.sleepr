#' Apply the Choi algorithm
#'
#' The Choi algorithm detects periods of non-wear in activity
#' data from an ActiGraph device. Such intervals are likely to
#' represent invalid data and therefore should be excluded from
#' downstream analysis.
#' @param agdb A `tibble` of activity data with an `epochlength` attribute.
#' The epoch length must be 60 seconds.
#' @inheritParams apply_troiano
#' @param min_period_len Minimum number of consecutive "zero" epochs
#' to start a non-wear period. The default is 90.
#' @param min_window_len The minimum number of consecutive "zero" epochs
#' immediately preceding and following a spike of artifactual movement.
#' The default is 30.
#' @details
#' The Choi algorithm extends the Troiano algorithm by requiring that
#' short spikes of artifactual movement during a non-wear period are
#' preceded and followed by `min_window_len` consecutive "zero" epochs.
#'
#' This implementation of the algorithm expects that the epochs are 60
#' second long.
#' @return A summary `tibble` of the detected non-wear periods.
#' If the activity data is grouped, then non-wear periods are
#' detected separately for each group.
#' @references L Choi, Z Liu, CE Matthews and MS Buchowski.
#' Validation of accelerometer wear and nonwear time classification
#' algorithm. *Medicine & Science in Sports & Exercise*,
#' 43(2):357–364, 2011.
#' @references ActiLife 6 User's Manual by the ActiGraph Software
#' Department. 04/03/2012.
#' @seealso [apply_troiano()], [collapse_epochs()]
#' @examples
#' library("dplyr")
#' data("gtxplus1day")
#'
#' gtxplus1day %>%
#'   collapse_epochs(60) %>%
#'   apply_choi()
#' @export

apply_choi <- function(agdb,
                       min_period_len = 90,
                       min_window_len = 30,
                       spike_tolerance = 2,
                       use_magnitude = FALSE) {
  check_args_nonwear_periods(agdb, "Choi", use_magnitude)
  assert_that(min_window_len >= spike_tolerance)

  nonwear <- agdb %>%
    group_modify(
      ~ apply_choi_(
        ., min_period_len, min_window_len, spike_tolerance, use_magnitude
      )
    )

  attr(nonwear, "nonwear_algorithm") <- "Choi"
  attr(nonwear, "min_period_len") <- min_period_len
  attr(nonwear, "min_window_len") <- min_window_len
  attr(nonwear, "spike_tolerance") <- spike_tolerance
  attr(nonwear, "use_magnitude") <- use_magnitude

  nonwear
}

apply_choi_ <- function(data,
                        min_period_len,
                        min_window_len,
                        spike_tolerance,
                        use_magnitude) {
  data %>%
    add_magnitude() %>%
    mutate(
      count = if (use_magnitude) .data$magnitude else .data$axis1,
      wear = as.integer(.data$count > 0)
    ) %>%
    group_by(
      rleid = rleid(.data$wear)
    ) %>%
    summarise(
      wear = first(.data$wear),
      timestamp = first(.data$timestamp),
      length = n()
    ) %>%
    # Let (spike, zero, zero, spike) -> (spike of length 4)
    # as long as (zero, zero) is shorter than spike_tolerance
    mutate(
      wear = if_else(
        .data$wear == 0L & .data$length < spike_tolerance,
        1L, .data$wear
      )
    ) %>%
    group_by(
      rleid = rleid(.data$wear)
    ) %>%
    summarise(
      wear = first(.data$wear),
      timestamp = first(.data$timestamp),
      length = sum(.data$length)
    ) %>%
    # Ignore artifactual movement intervals
    mutate(
      wear = if_else(
        .data$wear == 1L & .data$length <= spike_tolerance &
          lead(.data$length, default = 0L) >= min_window_len &
          lag(.data$length, default = 0L) >= min_window_len,
        0L, .data$wear
      )
    ) %>%
    group_by(
      rleid = rleid(.data$wear)
    ) %>%
    summarise(
      wear = first(.data$wear),
      timestamp = first(.data$timestamp),
      length = sum(.data$length)
    ) %>%
    filter(
      .data$wear == 0L,
      # TODO: Filtering if the row_number is 1 or n(),
      # regardless of the period length, means that
      # the initial and final non-wear periods can be shorter.
      .data$length >= min_period_len # | row_number() %in% c(1, n())
    ) %>%
    mutate(
      period_end = .data$timestamp + duration(.data$length, "mins")
    ) %>%
    select(
      period_start = .data$timestamp, .data$period_end, .data$length
    )
}
