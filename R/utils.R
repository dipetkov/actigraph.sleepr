#' Find the complement of time periods
#'
#' Find the complement of a set of time periods in a set of epochs.
#' For illustration, let's use integers instead of time periods and
#' epochs. Suppose we have two intervals/periods, `{[1,3], [8,10]}`;
#' their complement in the set `{[0, ..., 12]}` is `{[0,0], [4,7], [11,12]}`.
#' @param periods A `tibble` with at least two columns,
#' `start_var` and `end_var` which are the first
#' and the last epoch in a set of time periods, e.g. sleep
#' periods or (non)wear periods.
#' @param epochs A `tibble` with at least one column,
#' `timestamp`, which contains POSIXct objects.
#' @param start_var The variable (unquoted) which indicates
#' when the time periods start.
#' @param end_var The variable (unquoted) which indicates when
#' the time periods end.
#' @return A `tibble` of time periods with three columns:
#' `period_id` (a sequential identifier), `start_var`
#' (first epoch in period) and `end_var` (last epoch in period).
#' @examples
#' library("lubridate")
#' library("dplyr")
#' periods <- tibble(
#'   start = ymd_hm("2017-01-01 00:01"),
#'   end = ymd_hm("2017-01-01 00:05")
#' )
#' epochs <- tibble(timestamp = ymd_hm("2017-01-01 00:00") +
#'   minutes(0:12))
#' complement_periods(periods, epochs, start, end)
#' @export
complement_periods <- function(periods, epochs, start_var, end_var) {
  if (!nrow(periods)) {
    return(epochs |>
      summarise(
        period_start = first(.data$timestamp),
        period_end = last(.data$timestamp),
        length = time_length(.data$period_end - .data$period_start, "min")
      ))
  }
  start_var <- enquo(start_var)
  end_var <- enquo(end_var)
  combine_epochs_periods(epochs, periods, !!start_var, !!end_var) |>
    mutate(
      rev_id = rleid(is.na(.data$period_id))
    ) |>
    filter(
      is.na(.data$period_id)
    ) |>
    group_by(
      .data$rev_id,
      .add = TRUE
    ) |>
    summarise(
      period_start = first(.data$timestamp),
      period_end = last(.data$timestamp),
      length = n()
    ) |>
    select(
      -"rev_id"
    )
}

#' Expand a time period into a vector of equally spaced time points
#'
#' Given the start time and the end time of a period, expand it into a
#' vector of equally spaced time points.
#' @param start The start time, as a POSIXct object.
#' @param end The end time, as a POSIXct object.
#' @param units The time unit as a characters string.
#' The default is `"1 min"`.
#' @return A POSIXct vector of equally spaced time points starting at
#' `start`, ending no later than `end`, and separated by `units`.
#' @examples
#' start <- as.POSIXct("2017-01-01")
#' end <- as.POSIXct("2017-01-01 01:00:00")
#' expand_timestamp(start, end, "15 mins")
#' @export
expand_timestamp <- function(start, end, units = "1 min") {
  assert_that(
    is.POSIXct(start),
    is.POSIXct(end)
  )
  seq(start, end, by = units)
}

#' Expand time periods into a `tibble` of equally spaced time points
#' @inheritParams complement_periods
#' @param units The time unit as a characters string. The default is `"1 min"`.
#' @return A `tibble` with two columns: `period_id` identifies the input period
#' and `timestamp` is a POSIXct vector of time points within that period,
#' equally spaced by `units`. Any grouping of `periods` is preserved.
#' @examples
#' library("dplyr")
#' data("gtxplus1day")
#'
#' gtxplus1day |>
#'   collapse_epochs(60) |>
#'   apply_choi(min_period_len = 45) |>
#'   expand_periods(period_start, period_end, units = "30 mins")
#' @export
expand_periods <- function(periods, start_var, end_var,
                           units = "1 min") {
  start_var <- enquo(start_var)
  end_var <- enquo(end_var)
  periods |>
    group_modify(
      ~ expand_periods_(., !!start_var, !!end_var, units)
    )
}

expand_periods_ <- function(periods, start_var, end_var,
                            units = "1 min") {
  start_var <- enquo(start_var)
  end_var <- enquo(end_var)

  start <- pull(periods, !!start_var)
  end <- pull(periods, !!end_var)
  timestamps <- Map(
    expand_timestamp, start, end,
    MoreArgs = list(units = units)
  )
  tibble::tibble(
    period_id = rep(seq_along(timestamps), lengths(timestamps)),
    timestamp = .POSIXct(
      as.numeric(unlist(timestamps)),
      tz = attr(start, "tzone")
    )
  )
}

#' Guess the epoch length (in seconds) from the timestamp column
#' @param epochs A `tibble` with at least one column, `timestamp`,
#' which contains POSIXct objects.
#' @return The epoch length in seconds, inferred from the spacing between
#' consecutive timestamps. Throws an error if the time points are not
#' equally spaced.
#' @examples
#' data("gtxplus1day")
#'
#' gtxplus1day |>
#'   get_epoch_length()
#'
#' gtxplus1day |>
#'   collapse_epochs(60) |>
#'   get_epoch_length()
#' @export
get_epoch_length <- function(epochs) {
  assert_that(exists("timestamp", epochs),
    msg = "Tibble has no timestamp column."
  )

  epoch_lens <- time_length(epochs$timestamp - lag(epochs$timestamp))
  # The first length is `NA` by construction
  epoch_len <- epoch_lens[2]

  assert_that(
    epoch_len == last(epoch_lens) & epoch_len == mode(epoch_lens),
    msg = "Failed to determine epoch length from timestamps."
  )

  epoch_len
}

# Find the mode [the most common value].
mode <- function(x) {
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

# Remove the dependency on `data.table::rleid`.
rleid <- function(x) {
  x <- rle(x)$lengths
  rep(seq_along(x), times = x)
}

# Last observation carried forward. Replaces `zoo::na.locf()` with its
# default `na.rm = TRUE`: leading NAs (which have no prior observation)
# are dropped.
na_locf <- function(x) {
  x[cummax(seq_along(x) * !is.na(x))]
}

# Remove leading and trailing NAs. Replaces `zoo::na.trim()` with its
# defaults (`sides = "both"`, `is.na = "any"`): for a data frame, leading
# and trailing rows with an NA in any column are removed.
na_trim <- function(object) {
  ok <- stats::complete.cases(object)
  keep <- cumsum(ok) > 0 & rev(cumsum(rev(ok)) > 0)
  if (length(dim(object)) < 2L) {
    object[keep]
  } else {
    object[keep, , drop = FALSE]
  }
}

# Fill NAs by cubic spline interpolation. Replaces `zoo::na.spline()` on
# a vector with its defaults (Forsythe, Malcolm and Moler spline, indexed
# by position).
na_spline <- function(x) {
  ok <- !is.na(x)
  if (!any(ok)) {
    return(double())
  }
  idx <- seq_along(x)
  stats::splinefun(idx[ok], x[ok], method = "fmm")(idx)
}
