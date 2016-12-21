#' Apply the Cole-Kripke algorithm
#'
#' The Cole-Kripke sleep scoring algorithm is primarily used for adult populations as the supporting research was performed on subjects ranging from 35 to 65 years of age.
#' @inheritParams apply_sadeh
#' @return A \code{tibble} (\code{tbl}) of activity data. A new column \code{sleep} indicates whether each 60s epoch is scored as asleep (0) or awake (1).
#' @details
#' The Cole-Kripke algorithm requires that the activity data is in 60s epochs and uses a 7-minute window that includes the four previous and two future epochs. This function implements the algorithm as described in the ActiGraph user manual.
#'
#' The Cole-Kripke algorithm uses the y-axis (axis 1) counts. First epoch counts are divided by 100 and afterwards any scaled counts over 300 are set to 300. The sleep index (SI) is defined as
#'
#' \code{
#' .001 * (106 * Epoch_prev(4) + 54 * Epoch_prev(3) +
#'          58 * Epoch_prev(2) + 76 * Epoch_prev(1) +
#'         230 * Epoch +
#'          74 * Epoch_next(1) + 67 * Epoch_next(2))
#' }
#'
#' where at epoch t
#'
#' \describe{
#'   \item{Epoch_prev(i)}{the scaled activity count i epoch before t.}
#'   \item{Epoch_next(i)}{the scaled activity count i epochs after t.}
#' }
#'
#' The time series of activity counts is padded with zeros as necessary, at the beginning and at the end.
#'
#' Finally, the sleep state is awake (1) if the sleep index SI is less than 1; otherwise the sleep state is asleep (0).
#'
#' @references RJ Cole, DF Kripke, W Gruen, DJ Mullaney and JC Gillin. Automatic sleep/wake identification from wrist activity. \emph{Sleep}, 15(5):461–469, 1992.
#' @references ActiLife 6 User's Manual by the ActiGraph Software Department. 04/03/2012.
#' @seealso \code{\link{collapse_epochs}}, \code{\link{apply_sadeh}}, \code{\link{apply_tudor_locke}}
#' @examples
#' file <- system.file("extdata", "GT3XPlus-RawData-Day01-10sec.agd",
#'                     package = "actigraph.sleepr")
#' agdb_10s <- read_agd(file)
#' agdb_60s <- collapse_epochs(agdb_10s, 60)
#' agdb_scored <- apply_cole_kripke(agdb_60s)
#' @export

apply_cole_kripke <- function(agdb) {

  check_args_sleep_scores(agdb, "Cole-Kripke")
  attr(agdb, "sleep_algorithm") <- "Cole-Kripke"
  agdb %>% do(apply_cole_kripke_(.))
}

apply_cole_kripke_ <- function(data) {

  data %>%
    mutate(count = pmin(axis1 / 100, 300),
           sleep = .001 * (106 * lag(count, 4, default = 0) +
                             54 * lag(count, 3, default = 0) +
                             58 * lag(count, 2, default = 0) +
                             76 * lag(count, 1, default = 0) +
                             230 * count +
                             74 * lead(count, 1, default = 0) +
                             67 * lead(count, 2, default = 0)),
           sleep = if_else(sleep < 1, 0L, 1L))
}
