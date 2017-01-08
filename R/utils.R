#' Find the complement of time periods
#'
#' Find the complement of a set of time periods in a set of epochs. For illustration, let's use integers instead of time periods and epochs. Suppose we have two intervals/periods, \code{\{[1, 3], [8, 10]\}}; their complement in the set \code{\{[0, ..., 12]\}} is \code{\{[0, 0], [4, 7], [11, 12]\}}.
#' @param periods A data frame with at least two columns, \code{start_var} and \code{end_var}, which are the first and the last epoch in a set of time periods.
#' @param epochs A data frame with at least one column, \code{timestamp}, which contains consecutive 60s epochs.
#' @param start_var The column which indicates when the time periods start.
#' @param end_var The column which indicates when the time periods end.
#' @return A data frame of time periods with three columns: \code{period_id} (a sequential identifier), \code{start_var} (first epoch in period) and \code{end_var} (last epoch in period).
#' @examples
#' library("lubridate")
#' library("dplyr")
#' periods <- data_frame(start = ymd_hm("2017-01-01 00:01"),
#'                       end = ymd_hm("2017-01-01 00:05"))
#' epochs <- data_frame(timestamp = ymd_hm("2017-01-01 00:00") +
#'                        minutes(0:12))
#' complement_periods(periods, epochs, "start", "end")
#' @export
complement_periods <- function(periods, epochs, start_var, end_var) {
  stopifnot(exists("timestamp", where = epochs))
  stopifnot(exists(start_var, where = periods))
  stopifnot(exists(end_var, where = periods))

  join_vars <- "timestamp"
  if (is.grouped_df(epochs))
    join_vars <- c(join_vars, as.character(groups(epochs)))

  complement <- expand_periods(periods, start_var, end_var) %>%
    right_join(epochs, by = join_vars) %>%
    mutate(rev_id = rleid(is.na(period_id))) %>%
    filter(is.na(period_id)) %>%
    # Even if the epochs are grouped, the grouping gets lost now,
    # which is appropriate since we don't know if the grouping
    # applies to the complement of the periods
    group_by(rev_id) %>%
    summarise(period_start = first(timestamp),
              period_end = last(timestamp),
              length = n()) %>%
    ungroup() %>%
    select(period_start, period_end, length)

  class(complement) <- c("tbl_period", "tbl_df", "tbl", "data.frame")
  complement
}
expand_timestamp <- function(start, end, units = "min") {
  seq(start, end, by = units)
}
expand_periods <- function(periods, start_var, end_var) {
  stopifnot(exists(start_var, where = periods))
  stopifnot(exists(end_var, where = periods))
  periods %>%
    do(expand_periods_(., start_var, end_var))
}
expand_periods_ <- function(periods, start_var, end_var) {
  periods %>%
    mutate(period_id = row_number()) %>%
    mutate_(timestamp = interp(~ map2(start, end, expand_timestamp),
                               start = as.name(start_var),
                               end = as.name(end_var))) %>%
    select(period_id, timestamp) %>%
    unnest()
}
