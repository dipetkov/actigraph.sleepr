#' Apply the Sadeh algorithm
#'
#' The Sadeh sleep scoring algorithm is primarily used for younger adolescents as the supporting research was performed on children and young adults.
#' @param agdb A \code{tibble} (\code{tbl}) of activity data (at least) an \code{epochlength} attribute. The epoch length must be 60 seconds.
#' @return A \code{tibble} (\code{tbl}) of activity data. A new column \code{sleep} indicates whether each 60s epoch is scored as asleep (S) or awake (W).
#' @details
#' The Sadeh algorithm requires that the activity data is in 60s epochs and uses an 11-minute window that includes the five previous and five future epochs. This function implements the algorithm as described in the ActiGraph user manual.
#'
#' The Sadeh algorithm uses the y-axis (axis 1) counts; epoch counts over 300 are set to 300. The sleep index (SI) is defined as
#'
#' \code{
#' SI = 7.601 - (0.065 * AVG) - (1.08 * NATS) - (0.056 * SD) - (0.703 * LG)
#' }
#'
#' where at epoch t
#'
#' \describe{
#'   \item{AVG}{the arithmetic mean (average) of the activity counts in an 11-epoch window centered at \code{t}}
#'   \item{NATS}{the number of epochs in this 11-epoch window which have counts >= 50 and < 100}
#'   \item{SD}{the standard deviation of the counts in a 6-epoch window that includes \code{t} and the five preceding epochs}
#'   \item{LG}{the natural (base e) logarithm of the activity at epoch \code{t}. To avoid taking the log of 0, we add 1 to the count.}
#' }
#'
#' The time series of activity counts is padded with zeros as necessary, at the beginning and at the end, to compute the three functions AVG, SD, NATS within a rolling window.
#'
#' Finally, the sleep state is asleep (S) if the sleep index SI is greater than -4; otherwise the sleep state is awake (W).
#'
#' @references A Sadeh, KM Sharkey and MA Carskadon. Activity based sleep-wake identification: An empirical test of methodological issues. \emph{Sleep}, 17(3):201–207, 1994.
#' @references ActiLife 6 User's Manual by the ActiGraph Software Department. 04/03/2012.
#' @seealso \code{\link{collapse_epochs}}, \code{\link{apply_cole_kripke}}, \code{\link{apply_tudor_locke}}
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
  agdb %>% do(apply_sadeh_(.))
}

apply_sadeh_ <- function(data) {

  half_window <- 5
  roll_avg <- function(x) {
    zeros <- rep(0, half_window)
    roll_mean(c(zeros, x, zeros), n = 2 * half_window + 1, partial = FALSE)
  }
  roll_std <- function(x) {
    zeros <- rep(0, half_window)
    roll_sd(c(zeros, x), n = half_window + 1, partial = FALSE, align = "right")
  }
  roll_nats <- function(x) {
    zeros <- rep(0, half_window)
    y <- if_else(x >= 50 & x < 100, 1, 0)
    roll_sum(c(zeros, y, zeros), n = 2 * half_window + 1, partial = FALSE)
  }

  data %>%
    mutate(count = pmin(.data$axis1, 300),
           sleep = (7.601
                    - 0.065 * roll_avg(.data$count)
                    - 1.08 * roll_nats(.data$count)
                    - 0.056 * roll_std(.data$count)
                    - 0.703 * log(.data$count + 1)),
           sleep = if_else(.data$sleep > -4, "S", "W"))
}
