#' Apply the Cole-Kripke algorithm
#'
#' The Cole-Kripke sleep scoring algorithm is primarily used for adult
#' populations as the supporting research was performed on subjects
#' ranging from 35 to 65 years of age.
#' @inheritParams apply_sadeh
#' @return A \code{tibble} (\code{tbl}) of activity data. A new column
#' \code{sleep} indicates whether each 60s epoch is scored as asleep (S)
#' or awake (W).
#' @details
#' The original paper proposes three versions of the Cole-Kripke algorithm,
#' optimized for 1-minute, 30-second and 10-second epochs. Here only the 1-min
#' version is implemented and therefore the \code{apply_cole_kripke} function
#' requires that the activity data is in 60s epochs. Use the
#' \code{\link{collapse_epochs}} function to modify higher-frequency data,
#' if necessary.
#'
#' The Cole-Kripke algorithm uses the y-axis (axis 1) counts. First epoch
#' counts are divided by 100 and any scaled counts over 300 are clipped to 300.
#' This transformation is specific to ActiGraph devices. The sleep index (SI)
#' is defined as
#'
#' \code{
#' .001 * (106 * epoch_prev(4) + 54 * epoch_prev(3) +
#'          58 * epoch_prev(2) + 76 * epoch_prev(1) +
#'         230 * epoch +
#'          74 * epoch_next(1) + 67 * epoch_next(2))
#' }
#'
#' where at epoch \code{t}, \code{epoch_prev(i)} is the scaled activity count
#' \code{i} epochs before \code{t}. Similarly, \code{epoch_next(i)} is the
#' scaled activity count \code{i} epochs before \code{t}. That is, the algorithm
#' uses a 7-epoch window which includes the four preceding and the two
#' subsequent epochs. The time series of activity counts is padded with zeros as
#' necessary, at the beginning and at the end.
#'
#' Finally, the sleep state is awake (W) if the sleep index SI is less
#' than 1; otherwise the sleep state is asleep (S).
#'
#' @references RJ Cole, DF Kripke, W Gruen, DJ Mullaney and JC Gillin.
#' Automatic sleep/wake identification from wrist activity.
#' \emph{Sleep}, 15(5):461–469, 1992.
#' @references ActiLife 6 User's Manual by the ActiGraph Software
#' Department. 04/03/2012.
#' @seealso \code{\link{collapse_epochs}}, \code{\link{apply_sadeh}},
#' \code{\link{apply_tudor_locke}}
#' @examples
#' library("dplyr")
#' data("gtxplus1day")
#'
#' gtxplus1day %>%
#'   collapse_epochs(60) %>%
#'   apply_cole_kripke()
#' @export

apply_cole_kripke <- function(agdb) {
  check_args_sleep_scores(agdb, "Cole-Kripke")
  attr(agdb, "sleep_algorithm") <- "Cole-Kripke"
  agdb %>%
    actigraph_adjustment() %>%
    do(apply_cole_kripke_1min_(.))
}

apply_cole_kripke_1min_ <- function(data) {
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
      sleep = if_else(.data$sleep < 1, "S", "W")
    )
}

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

apply_cole_kripke_10sec_ <- function(data) {
  data %>%
    mutate(
      count = pmin(.data$axis1 / 100, 300),
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
