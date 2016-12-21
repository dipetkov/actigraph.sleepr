#' Combine epochs with sleep/nonwear periods
#'
#' Suppose we have used \code{apply_tudor_locke} to detect sleep periods or \code{apply_troiano}/\code{apply_choi} to detect non-wear periods. It might be useful to combine the epochs data with the periods data, so that each epoch is labeled according to which period it falls into, if any. Then we can easily slice the epochs data by sleep/non-sleep or wear/non-wear.
#' @param epochs A \code{tibble} (\code{tbl}) of activity data with (at least) a timestamp column. The epoch length must be 60 seconds.
#' @param periods A summary \code{tibble} of the (sleep or non-wear) periods.
#' @param time_start_var The \code{periods} column which specifies when the periods start.
#' @param time_len_var The \code{periods} column which specifies how long the periods last, in minutes.
#' @param rev Logical. If \code{rev = TRUE}, then label the epochs outside of the given periods.
#' @return A \code{tibble} of activity data with one additional column, \code{period_id}, which indicates the period each epoch falls into.
#' @examples
#' file <- system.file("extdata", "GT3XPlus-RawData-Day01-10sec.agd",
#'                     package = "actigraph.sleepr")
#' agdb_10s <- read_agd(file)
#' agdb_60s <- collapse_epochs(agdb_10s, 60)
#' agdb_scored <- apply_sadeh(agdb_60s)
#' periods_sleep <- apply_tudor_locke(agdb_scored,
#'                                    min_sleep_period = 60)
#' agdb_sleep <- combine_epochs_periods(agdb_scored, periods_sleep,
#'                                      time_start_var = "in_bed_timestamp",
#'                                      time_len_var = "time_in_bed")
#'
#' # How many sleep periods were detected and what is their duration, in minutes?
#' periods_sleep$time_in_bed
#' # What is the assignment of epochs to periods?
#' table(agdb_sleep$period_id)
#' @export
combine_epochs_periods <- function(epochs, periods,
                                   time_start_var, time_len_var,
                                   rev = FALSE) {

  stopifnot(inherits(epochs, "tbl_agd"))
  stopifnot(inherits(periods, "tbl_period"))
  stopifnot(attr(epochs, "epochlength") == 60)
  stopifnot(exists(time_start_var, where = periods))
  stopifnot(exists(time_len_var, where = periods))
  stopifnot(is.integer(periods[[time_len_var]]))

  filter_criteria <- interp(~ var > 0, var = as.name(time_len_var))
  periods_nonzero <- periods %>% filter_(filter_criteria)

  if (nrow(periods_nonzero) == 0) {
    if (rev)
      return(epochs %>% mutate(period_id = 1L))
    else
      return(epochs %>% mutate(period_id = NA_integer_))
  }

  expand_timestamp <- function(timestamp, len) {
    timestamp + duration(seq_len(len) - 1, units = "mins")
  }

  periods_nonzero <- periods_nonzero %>%
    mutate(period_id = row_number()) %>%
    rowwise() %>%
    do({
      data_frame(period_id = .$period_id,
                 timestamp = expand_timestamp(.[[time_start_var]],
                                              .[[time_len_var]]))
    }) %>%
    ungroup()

  if (exists("period_id", epochs))
    epochs_periods <- epochs %>% select(- period_id)
  else
    epochs_periods <- epochs

  epochs_periods <- epochs_periods %>%
    left_join(periods_nonzero, by = "timestamp")

  if (rev) {
    epochs_periods <- epochs_periods %>%
      mutate(rev_id = na.fill(na.locf(period_id, na.rm = FALSE), 0),
             rev_id = as.integer(rev_id),
             period_id = if_else(is.na(period_id), rev_id, NA_integer_)) %>%
      select(- rev_id)
  }

  class(epochs_periods) <- class(epochs)
  for (a in setdiff(names(attributes(epochs)),
                    special_dplyr_attributes())) {
    attr(epochs_periods, a) <- attr(epochs, a)
  }
  epochs_periods
}
