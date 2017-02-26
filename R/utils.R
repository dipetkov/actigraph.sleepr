#' Find the complement of time periods
#'
#' Find the complement of a set of time periods in a set of epochs. For illustration, let's use integers instead of time periods and epochs. Suppose we have two intervals/periods, \code{\{[1, 3], [8, 10]\}}; their complement in the set \code{\{[0, ..., 12]\}} is \code{\{[0, 0], [4, 7], [11, 12]\}}.
#' @param periods A data frame with at least two columns, \code{start_var} and \code{end_var}, which are the first and the last epoch in a set of time periods, e.g. sleep periods or (non)wear periods.
#' @param epochs A data frame with at least one column, \code{timestamp}, which contains POSIXct objects.
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

  epoch_len <- epochs %>%
    mutate(epoch_len = time_length(timestamp - lag(timestamp),
                                   unit = "second")) %>%
    select(epoch_len) %>% na.omit() %>% .$epoch_len
  stopifnot(n_distinct(epoch_len) == 1)
  units <- paste(epoch_len[1], "sec")

  # TODO: a more general approach to collapsing
  # might use the findInterval function
  # though care must be taken with "incomplete"
  # epochs at the start/end of the time series

  join_vars <- "timestamp"
  if (is.grouped_df(epochs))
    join_vars <- c(join_vars, as.character(groups(epochs)))

  complement <- expand_periods(periods, start_var, end_var,
                               units = units) %>%
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
#' file <- system.file("extdata", "GT3XPlus-RawData-Day01-10sec.agd",
#'                     package = "actigraph.sleepr")
#' agdb_10s <- read_agd(file)
#' agdb_60s <- collapse_epochs(agdb_10s, 60)
#' periods_nonwear <- apply_choi(agdb_60s,  min_period_len = 45)
#' periods_nonwear
#'
#' expand_periods(periods_nonwear, "period_start", "period_end",
#'                units = "30 mins")
#' @export
expand_periods <- function(periods, start_var, end_var,
                           units = "1 min") {
  stopifnot(exists(start_var, where = periods),
            exists(end_var, where = periods),
            is.POSIXct(periods[[start_var]]),
            is.POSIXct(periods[[end_var]]))
  periods %>%
    do(expand_periods_(., start_var, end_var, units))
}
expand_periods_ <- function(periods, start_var, end_var,
                            units = "1 min") {
  periods %>%
    mutate(period_id = row_number()) %>%
    mutate_(timestamp = interp(~ map2(start, end, expand_timestamp, units),
                               start = as.name(start_var),
                               end = as.name(end_var))) %>%
    select(period_id, timestamp) %>%
    unnest()
}
