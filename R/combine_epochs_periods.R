#' Combine epochs with sleep/nonwear periods
#'
#' Suppose we have used [apply_tudor_locke()] to detect sleep periods
#' or [apply_troiano()]/[apply_choi()] to detect non-wear periods.
#' It might be useful to combine the epochs data with the periods data, so
#' that each epoch is labeled according to which period it falls into, if
#' any. Then we can easily slice the epochs data by sleep/non-sleep or
#' wear/non-wear.
#' @param epochs A `tibble` of activity data with a `timestamp` column.
#' @param periods A summary `tibble` of the (sleep or non-wear) periods.
#' @param start_var The `periods` column which specifies when the
#' periods start.
#' @param end_var The `periods` column which specifies when the
#' periods end.
#' @return A `tibble` of activity data with one additional column,
#' `period_id`, which indicates the period each epoch falls into.
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
#' agdb_with_periods <- combine_epochs_periods(
#'   agdb, periods,
#'   in_bed_time, out_bed_time
#' )
#'
#' # How many sleep periods were detected and what is their duration,
#' # in minutes?
#' periods %>% select(in_bed_time, out_bed_time, duration)
#' # What is the assignment of epochs to periods?
#' agdb_with_periods %>% count(period_id)
#' @export
combine_epochs_periods <- function(epochs, periods, start_var, end_var) {
  start_var <- enquo(start_var)
  end_var <- enquo(end_var)

  units <- get_epoch_length(epochs)
  assert_that(is_scalar_integerish(units))

  periods_by_minute <-
    expand_periods(periods, !!start_var, !!end_var, units = units)

  left_join(
    epochs, periods_by_minute,
    by = c("timestamp", group_vars(epochs))
  )
}
