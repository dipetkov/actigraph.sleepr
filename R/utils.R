#' Find the complement of time periods
#'
#' Find the complement of a set of time periods in a set of epochs. For illustration, let's use integers instead of time periods and epochs. Suppose we have two intervals/periods, \code{\{[1, 3], [8, 10]\}}; their complement in the set \code{\{[0, ..., 12]\}} is \code{\{[0, 0], [4, 7], [11, 12]\}}.
#' @param periods A data frame with at least two columns, \code{start_var} and \code{end_var}, which are the first and the last epoch in a set of time periods, e.g. sleep periods or (non)wear periods.
#' @param epochs A data frame with at least one column, \code{timestamp}, which contains POSIXct objects.
#' @param start_var The variable (unquoted) which indicates when the time periods start.
#' @param end_var The variable (unquoted) which indicates when the time periods end.
#' @return A data frame of time periods with three columns: \code{period_id} (a sequential identifier), \code{start_var} (first epoch in period) and \code{end_var} (last epoch in period).
#' @examples
#' library("lubridate")
#' library("dplyr")
#' periods <- data_frame(start = ymd_hm("2017-01-01 00:01"),
#'                       end = ymd_hm("2017-01-01 00:05"))
#' epochs <- data_frame(timestamp = ymd_hm("2017-01-01 00:00") +
#'                        minutes(0:12))
#' complement_periods(periods, epochs, start, end)
#' @export
complement_periods <- function(periods, epochs, start_var, end_var) {
  if (!nrow(periods))
    return(epochs %>%
             summarise(period_start = first(.data$timestamp),
                       period_end = last(.data$timestamp),
                       length = time_length(.data$period_end -
                                              .data$period_start, "min")))
  start_var <- enquo(start_var)
  end_var <- enquo(end_var)
  combine_epochs_periods(epochs, periods, !!start_var, !!end_var) %>%
    mutate(rev_id = rleid(is.na(.data$period_id))) %>%
    filter(is.na(.data$period_id)) %>%
    group_by(.data$rev_id, add = TRUE) %>%
    summarise(period_start = first(.data$timestamp),
              period_end = last(.data$timestamp),
              length = n()) %>%
    select(- .data$rev_id)
}

#' Expand a time period into a vector of equally spaced time points
#'
#' Given the start time and the end time of a period, expand it into a vector of equally spaced time points.
#' @param start The start time, as a POSIXct object.
#' @param end The end time, as a POSIXct object.
#' @param units The time unit as a characters string. The default is \code{"1 min"}.
#' @examples
#' start <- as.POSIXct("2017-01-01")
#' end <- as.POSIXct("2017-01-01 01:00:00")
#' expand_timestamp(start, end, "15 mins")
#' @export
expand_timestamp <- function(start, end, units = "1 min") {
  stopifnot(is.POSIXct(start), is.POSIXct(end))
  seq(start, end, by = units)
}

#' Expand time periods into a data frame of equally spaced time points
#' @inheritParams complement_periods
#' @param units The time unit as a characters string. The default is \code{"1 min"}.
#' @examples
#' library("dplyr")
#' data("gtxplus1day")
#'
#' gtxplus1day %>%
#'   collapse_epochs(60) %>%
#'   apply_choi(min_period_len = 45) %>%
#'   expand_periods(period_start, period_end, units = "30 mins")
#' @export
expand_periods <- function(periods, start_var, end_var,
                           units = "1 min") {
  start_var <- enquo(start_var)
  end_var <- enquo(end_var)
  periods %>%
    do(expand_periods_(., !!start_var, !!end_var, units))
}
expand_periods_ <- function(periods, start_var, end_var,
                            units = "1 min") {
  start_var <- enquo(start_var)
  end_var <- enquo(end_var)
  periods %>%
    mutate(period_id = row_number()) %>%
    mutate(timestamp = map2(!!start_var, !!end_var, expand_timestamp,
                            units)) %>%
    select(.data$period_id, .data$timestamp) %>%
    unnest()
}
get_epoch_length <- function(epochs) {

  if (!exists("timestamp", epochs)) return(NULL)

  epoch_len <- epochs %>%
    mutate(len = time_length(.data$timestamp - lag(.data$timestamp))) %>%
    filter(row_number() > 1) %>%
    .$len

  if (n_distinct(epoch_len) != 1) NULL else first(epoch_len)
}
