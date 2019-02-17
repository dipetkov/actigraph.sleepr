#' Combine epochs with sleep/nonwear periods
#'
#' Suppose we have used \code{apply_tudor_locke} to detect sleep periods or \code{apply_troiano}/\code{apply_choi} to detect non-wear periods. It might be useful to combine the epochs data with the periods data, so that each epoch is labeled according to which period it falls into, if any. Then we can easily slice the epochs data by sleep/non-sleep or wear/non-wear.
#' @param epochs A \code{tibble} (\code{tbl}) of activity data with (at least) a timestamp column.
#' @param periods A summary \code{tibble} of the (sleep or non-wear) periods.
#' @param start_var The \code{periods} column which specifies when the periods start.
#' @param end_var The \code{periods} column which specifies when the periods end.
#' @return A \code{tibble} of activity data with one additional column, \code{period_id}, which indicates the period each epoch falls into.
#' @examples
#' library("dplyr")
#' data("gtxplus1day")
#'
#' agdb <- gtxplus1day %>%
#'   collapse_epochs(60) %>%
#'   apply_sadeh()
#' periods <- agdb %>%
#'   apply_tudor_locke(min_sleep_period = 60)
#'
#' agdb_with_periods <- combine_epochs_periods(agdb, periods,
#'                                             in_bed_time, out_bed_time)
#'
#' # How many sleep periods were detected and what is their duration, in minutes?
#' periods %>% select(in_bed_time, out_bed_time, duration)
#' # What is the assignment of epochs to periods?
#' agdb_with_periods %>% count(period_id)
#' @export
combine_epochs_periods <- function(epochs, periods, start_var, end_var) {

  start_var <- enquo(start_var)
  end_var <- enquo(end_var)

  units <- get_epoch_length(epochs)
  assert_that(is_scalar_integerish(units))

  epochs_periods <-
    expand_periods(periods, !!start_var, !!end_var, units = units) %>%
    right_join(epochs, by = c("timestamp", group_vars(epochs)))

  epochs_periods
}
