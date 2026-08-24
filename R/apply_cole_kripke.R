#' Apply the Cole-Kripke algorithm
#'
#' The Cole-Kripke sleep scoring algorithm is primarily used for adult
#' populations as the supporting research was performed on subjects
#' ranging from 35 to 65 years of age.
#' @inheritParams apply_sadeh
#' @param rescoring Logical; apply Webster's rescoring rules to the initial
#'   Cole-Kripke scores. The default, `FALSE`, preserves the original
#'   Cole-Kripke output and the historical behavior of this function.
#' @return A `tibble` of activity data. A new column `sleep` indicates
#' whether each 60s epoch is scored as asleep (S) or awake (W).
#' @details
#' The original paper proposes three versions of the Cole-Kripke algorithm,
#' optimized for 1-minute, 30-second and 10-second epochs. Here only the 1-min
#' version is implemented and therefore the [apply_cole_kripke()] function
#' requires that the activity data is in 60s epochs. Use the [collapse_epochs()]
#' function to modify higher-frequency data, if necessary.
#'
#' The Cole-Kripke algorithm uses the y-axis (axis 1) counts. First epoch
#' counts are divided by 100 and any scaled counts over 300 are clipped to 300.
#' This transformation is specific to ActiGraph devices. The sleep index (SI)
#' is defined as
#' ```
#' .001 * (106 * epoch_prev(4) + 54 * epoch_prev(3) +
#'          58 * epoch_prev(2) + 76 * epoch_prev(1) +
#'         230 * epoch +
#'          74 * epoch_next(1) + 67 * epoch_next(2))
#' ```
#' where at epoch `t`, `epoch_prev(i)` is the scaled activity count `i` epochs
#' *before* `t`. Similarly, `epoch_next(i)` is the scaled activity count `i`
#' epochs *after* `t`. That is, the algorithm  uses a 7-epoch window which
#' includes the four preceding and the two subsequent epochs. The time series
#' of activity counts is padded with zeros as necessary, at the beginning and
#' at the end.
#'
#' Finally, the sleep state is asleep (S) if the sleep index SI is less
#' than 1; otherwise the sleep state is awake (W).
#'
#' Set `rescoring = TRUE` to apply Webster's rules to the initial scores:
#' the first 1, 3, or 4 sleep minutes after respectively 4, 10, or 15 wake
#' minutes are rescored awake; sleep runs of at most 6 minutes surrounded by
#' 10 wake minutes on both sides, and runs of at most 10 minutes surrounded by
#' 20 wake minutes on both sides, are also rescored awake. These rules are
#' applied simultaneously to the initial scores, as in pyActigraphy.
#'
#' @references RJ Cole, DF Kripke, W Gruen, DJ Mullaney and JC Gillin.
#' Automatic sleep/wake identification from wrist activity.
#' *Sleep*, 15(5):461–469, 1992.
#' @references JB Webster, DF Kripke, S Messin, DJ Mullaney and G Wyborney.
#' An activity-based sleep monitor system for ambulatory use.
#' *Sleep*, 5(4):389–399, 1982. <https://doi.org/10.1093/sleep/5.4.389>.
#' @references ActiLife 6 User's Manual by the ActiGraph Software
#' Department. 04/03/2012.
#' @seealso [collapse_epochs()], [apply_sadeh()], [apply_tudor_locke()]
#' @examples
#' library("dplyr")
#' data("gtxplus1day")
#'
#' gtxplus1day %>%
#'   collapse_epochs(60) %>%
#'   apply_cole_kripke()
#' @export
apply_cole_kripke <- function(agdb, rescoring = FALSE) {
  check_args_sleep_scores(agdb, "Cole-Kripke")
  assert_that(is.logical(rescoring), length(rescoring) == 1L, !is.na(rescoring),
    msg = "`rescoring` must be TRUE or FALSE."
  )

  attr(agdb, "sleep_algorithm") <- "Cole-Kripke"

  agdb %>%
    actigraph_adjustment() %>%
    group_modify(
      ~ apply_cole_kripke_1min_(., rescoring)
    )
}

# The optimal parameters for the mean activity per minute.
# pg. 466, Sleep, Vol. 15, No. 5, 1992.
apply_cole_kripke_1min_ <- function(data, rescoring = FALSE) {
  data %>%
    mutate(
      sleep = .001 * (
        106 * lag(.data$count, 4, default = 0) +
          54 * lag(.data$count, 3, default = 0) +
          58 * lag(.data$count, 2, default = 0) +
          76 * lag(.data$count, 1, default = 0) +
          230 * .data$count +
          74 * lead(.data$count, 1, default = 0) +
          67 * lead(.data$count, 2, default = 0)),
      sleep = if_else(.data$sleep < 1, "S", "W"),
      sleep = if (rescoring) webster_rescore(.data$sleep) else .data$sleep
    )
}

# Webster et al. (1982) rescoring rules, applied to the initial scoring.
# Each rule uses the unrescored sequence; this matches the reference
# implementation in pyActigraphy.
webster_rescore <- function(sleep) {
  sleep <- as.character(sleep)
  r <- rle(sleep == "S")
  ends <- cumsum(r$lengths)
  starts <- ends - r$lengths + 1L
  rescore <- rep(FALSE, length(sleep))

  for (i in seq_along(r$lengths)) {
    if (!r$values[i]) {
      next
    }
    run_start <- starts[i]
    run_end <- ends[i]
    run_length <- r$lengths[i]
    preceding_wake <- if (i > 1L && !r$values[i - 1L]) r$lengths[i - 1L] else 0L
    following_wake <- if (i < length(r$lengths) && !r$values[i + 1L]) r$lengths[i + 1L] else 0L

    if (preceding_wake >= 4L && run_length >= 1L) {
      rescore[run_start] <- TRUE
    }
    if (preceding_wake >= 10L && run_length >= 3L) {
      rescore[run_start:(run_start + 2L)] <- TRUE
    }
    if (preceding_wake >= 15L && run_length >= 4L) {
      rescore[run_start:(run_start + 3L)] <- TRUE
    }
    if (run_length <= 6L && preceding_wake >= 10L && following_wake >= 10L) {
      rescore[run_start:run_end] <- TRUE
    }
    if (run_length <= 10L && preceding_wake >= 20L && following_wake >= 20L) {
      rescore[run_start:run_end] <- TRUE
    }
  }

  sleep[rescore] <- "W"
  sleep
}

# The optimal parameters for the maximum 30-second nonoverlapping epoch of
# activity per minute.
# pg. 466, Sleep, Vol. 15, No. 5, 1992.
apply_cole_kripke_30sec_ <- function(data) {
  data %>%
    mutate(
      sleep = .0001 * (
        50 * lag(.data$count, 4, default = 0) +
          30 * lag(.data$count, 3, default = 0) +
          14 * lag(.data$count, 2, default = 0) +
          28 * lag(.data$count, 1, default = 0) +
          121 * .data$count +
          8 * lead(.data$count, 1, default = 0) +
          50 * lead(.data$count, 2, default = 0)),
      sleep = if_else(.data$sleep < 1, "S", "W")
    )
}

# The optimal parameters for the maximum 10-second nonoverlapping epoch of
# activity per minute.
# pg. 466, Sleep, Vol. 15, No. 5, 1992.
apply_cole_kripke_10sec_ <- function(data) {
  data %>%
    mutate(
      sleep = .00001 * (
        550 * lag(.data$count, 4, default = 0) +
          378 * lag(.data$count, 3, default = 0) +
          413 * lag(.data$count, 2, default = 0) +
          699 * lag(.data$count, 1, default = 0) +
          1736 * .data$count +
          287 * lead(.data$count, 1, default = 0) +
          309 * lead(.data$count, 2, default = 0)),
      sleep = if_else(.data$sleep < 1, "S", "W")
    )
}

actigraph_adjustment <- function(data) {
  data %>%
    mutate(
      count = pmin(.data$axis1 / 100, 300)
    )
}
