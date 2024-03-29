#' Apply the Troiano algorithm
#'
#' The Troiano algorithm detects periods of non-wear in activity data
#' from an ActiGraph device. Such intervals are likely to represent
#' invalid data and therefore should be excluded from downstream
#' analysis. The algorithm formalizes a technique used to analyze the
#' 2003-2004 NHANES data; the original SAS source code can be found at
#' <https://riskfactor.cancer.gov/tools/nhanes_pam/>.
#' @param agdb A `tibble` of activity data with an `epochlength` attribute.
#' The epoch length must be 60 seconds.
#' @param activity_threshold Highest activity level to be considered
#' "zero"; an epoch with activity exceeding the threshold is considered
#' a "spike". The default threshold is 0.
#' @param min_period_len Minimum number of consecutive "zero" epoch to
#' start a non-wear period. The default is 60.
#' @param max_nonzero_count Epochs with activity greater than
#' `max_nonzero_count` are labeled as "zero". The default is Inf.
#' @param spike_tolerance Also known as artifactual movement interval.
#' At most `spike_tolerance` "nonzero" epochs can occur in sequence
#' during a non-wear period without interrupting it. The default is 2.
#' @param spike_stoplevel An activity spike that exceeds `spike_stoplevel`
#' counts ends a non-wear period, even if the spike tolerance has not been
#' reached. The default is 100.
#' @param use_magnitude Logical. If true, the magnitude of the vector
#' (axis1, axis2, axis3) is used to measure activity; otherwise the axis1
#' value is used. The default is FALSE.
#' @param endat_nnz_seq Logical. If true, a non-wear period ends with a run
#' of nonzero epochs that is longer than `spike_tolerance`. This
#' corresponds to the option *"Require consecutive epochs outside of
#' the activity threshold"* in ActiLife's Wear Time Validation menu.
#' The default is `TRUE`.
#' @details
#' The Troiano algorithm specifies that a non-wear period starts with
#' `min_period_len` consecutive epochs/minutes of "zero" activity
#' and ends with more than `spike_tolerance` epochs/minutes of
#' "nonzero" activity.
#'
#' This implementation of the algorithm expects 60s epochs.
#' @return A summary `tibble` of the detected non-wear periods.
#' If the activity data is grouped, then non-wear periods are detected
#' separately for each group.
#' @references RP Troiano, D Berrigan, KW Dodd, LC Mâsse, T Tilert and
#' M McDowell. Physical activity in the united states measured by
#' accelerometer. *Medicine & Science in Sports & Exercise*,
#' 40(1):181–188, 2008.
#' @references ActiLife 6 User's Manual by the ActiGraph Software
#' Department. 04/03/2012.
#' @seealso [apply_choi()], [collapse_epochs()]
#' @examples
#' library("dplyr")
#' data("gtxplus1day")
#'
#' gtxplus1day %>%
#'   collapse_epochs(60) %>%
#'   apply_troiano()
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

  if (endat_nnz_seq) {
    nonwear <- agdb %>%
      group_modify(
        ~ apply_troiano_seq_(
          ., activity_threshold,
          min_period_len, max_nonzero_count,
          spike_tolerance,
          spike_stoplevel,
          use_magnitude
        )
      )
  } else {
    nonwear <- agdb %>%
      group_modify(
        ~ apply_troiano_nonseq_(
          ., activity_threshold,
          min_period_len, max_nonzero_count,
          spike_tolerance,
          spike_stoplevel,
          use_magnitude
        )
      )
  }

  attr(nonwear, "nonwear_algorithm") <- "Troiano"
  attr(nonwear, "min_period_len") <- min_period_len
  attr(nonwear, "max_nonzero_count") <- max_nonzero_count
  attr(nonwear, "spike_tolerance") <- spike_tolerance
  attr(nonwear, "spike_stoplevel") <- spike_stoplevel
  attr(nonwear, "activity_threshold") <- activity_threshold
  attr(nonwear, "endat_nnz_seq") <- endat_nnz_seq
  attr(nonwear, "use_magnitude") <- use_magnitude

  nonwear
}

apply_troiano_seq_ <- function(data,
                               activity_threshold,
                               min_period_len, max_nonzero_count,
                               spike_tolerance,
                               spike_stoplevel,
                               use_magnitude) {
  data %>%
    add_magnitude() %>%
    mutate(
      count = if (use_magnitude) .data$magnitude else .data$axis1,
      wear = case_when(
        .data$count > spike_stoplevel ~ 2L,
        .data$count > activity_threshold &
          .data$count <= max_nonzero_count ~ 1L,
        TRUE ~ 0L
      )
    ) %>%
    group_by(
      rleid = rleid(.data$wear)
    ) %>%
    summarise(
      wear = first(.data$wear),
      timestamp = first(.data$timestamp),
      length = n()
    ) %>%
    mutate(
      wear = if_else(
        .data$wear == 1L &
          lead(.data$wear, default = 1L) == 0L &
          .data$length <= spike_tolerance,
        NA_integer_, .data$wear
      ),
      # Since `na.locf` can't impute leading NAs, fill in those with 1s
      wear = if_else(row_number() == 1 & is.na(.data$wear), 1L, .data$wear),
      # Fill in NAs with the most recent zero/nonzero wear state
      wear = na.locf(.data$wear)
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
      .data$length >= min_period_len
    ) %>%
    mutate(
      period_end = .data$timestamp + duration(.data$length, "mins")
    ) %>%
    select(
      period_start = .data$timestamp, .data$period_end, .data$length
    )
}

apply_troiano_nonseq_ <- function(data,
                                  activity_threshold,
                                  min_period_len, max_nonzero_count,
                                  spike_tolerance,
                                  spike_stoplevel,
                                  use_magnitude) {
  data %>%
    add_magnitude() %>%
    mutate(
      count = if (use_magnitude) .data$magnitude else .data$axis1,
      count = if_else(.data$count > max_nonzero_count, 0L, .data$count),
      length = wle(
        .data$count, activity_threshold, spike_tolerance, spike_stoplevel
      )
    ) %>%
    filter(
      .data$length >= min_period_len
    ) %>%
    select(
      period_start = .data$timestamp, .data$length
    ) %>%
    mutate(
      period_end = .data$period_start + duration(.data$length, "mins")
    ) %>%
    mutate(
      a = time_length(.data$period_start - first(.data$period_start), "min"),
      b = time_length(.data$period_end - first(.data$period_start), "min")
    ) %>%
    # Remove periods which overlap with previous periods
    filter(
      overlap(.data$a, .data$b)
    ) %>%
    select(
      .data$period_start, .data$period_end, .data$length
    )
}
