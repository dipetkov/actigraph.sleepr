#' Apply the Sadeh algorithm
#'
#' The Sadeh sleep scoring algorithm is primarily used for younger
#' adolescents as the supporting research was performed on children
#' and young adults.
#' @param agdb A `tibble` of activity data with an `epochlength` attribute.
#' The epoch length must be 60 seconds.
#' @return A `tibble` of activity data. A new column `sleep` indicates whether
#' each 60s epoch is scored as asleep (S) or awake (W).
#' @details
#' The Sadeh algorithm requires that the activity data is in 60s
#' epochs and uses an 11-minute window that includes the five previous
#' and five future epochs. This function implements the algorithm as
#' described in the ActiGraph user manual.
#'
#' The Sadeh algorithm uses the y-axis (axis 1) counts; epoch counts
#' over 300 are set to 300. The sleep index (SI) is defined as
#' ```
#' SI = 7.601 - (0.065 * AVG) - (1.08 * NATS) - (0.056 * SD) - (0.703 * LG)
#' ```
#' where at epoch `t`
#'
#' \describe{
#'   \item{AVG}{the arithmetic mean (average) of the activity counts in
#'   an 11-epoch window centered at `t`}
#'   \item{NATS}{the number of epochs in this 11-epoch window which
#'   have counts >= 50 and < 100}
#'   \item{SD}{the standard deviation of the counts in a 6-epoch
#'   window that includes `t` and the five preceding epochs}
#'   \item{LG}{the natural (base e) logarithm of the activity at
#'   epoch `t`. To avoid taking the log of 0, we add 1 to the count.}
#' }
#'
#' The time series of activity counts is padded with zeros as
#'  necessary, at the beginning and at the end, to compute the three
#'  functions AVG, SD, NATS within a rolling window.
#'
#' Finally, the sleep state is asleep (S) if the sleep index SI
#' is greater than -4; otherwise the sleep state is awake (W).
#'
#' @references A Sadeh, KM Sharkey and MA Carskadon. Activity
#' based sleep-wake identification: An empirical test of methodological
#' issues. *Sleep*, 17(3):201–207, 1994.
#' @references ActiLife 6 User's Manual by the ActiGraph Software
#' Department. 04/03/2012.
#' @seealso [apply_cole_kripke()], [apply_cole_kripke()], [apply_tudor_locke()]
#' @examples
#' library("dplyr")
#' data("gtxplus1day")
#'
#' gtxplus1day %>%
#'   collapse_epochs(60) %>%
#'   apply_sadeh()
#' @export

apply_sadeh <- function(agdb) {
  check_args_sleep_scores(agdb, "Sadeh")

  attr(agdb, "sleep_algorithm") <- "Sadeh"

  agdb %>% group_modify(
    ~ apply_sadeh_(.)
  )
}

apply_sadeh_ <- function(data) {
  half_window <- 5
  # From the Sadeh paper:
  # Mean-W-5-min is the average number of activity counts during the scored
  # epoch and the window of five epochs preceding and following it.
  roll_avg <- function(x) {
    padding <- rep(0, half_window)
    roll_mean(
      c(padding, x, padding),
      n = 2 * half_window + 1, align = "center", partial = FALSE
    )
  }
  # From the Sadeh paper:
  # SD-last 6 min is the standard deviation of the activity counts during
  # the scored epoch and the five epochs preceding it.
  roll_std <- function(x) {
    padding <- rep(0, half_window)
    roll_sd(c(padding, x),
      n = half_window + 1, align = "right", partial = FALSE
    )
  }
  # From the Sadeh paper:
  # NAT is the number of epochs with activity level equal to or higher than
  # 50 but lower than 100 activity counts in a window of 11 minutes that
  # includes the scored epoch and the five epochs preceding and following it.
  roll_nats <- function(x) {
    padding <- rep(0, half_window)
    y <- if_else(x >= 50 & x < 100, 1, 0)
    roll_sum(
      c(padding, y, padding),
      n = 2 * half_window + 1, align = "center", partial = FALSE
    )
  }

  data %>%
    mutate(
      count = pmin(.data$axis1 / 100, 300),
      sleep = (
        7.601
        - 0.065 * roll_avg(.data$count)
          - 1.08 * roll_nats(.data$count)
          - 0.056 * roll_std(.data$count)
          - 0.703 * log(.data$count + 1)),
      sleep = if_else(.data$sleep > -4, "S", "W")
    )
}
