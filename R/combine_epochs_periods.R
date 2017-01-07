#' Combine epochs with sleep/nonwear periods
#'
#' Suppose we have used \code{apply_tudor_locke} to detect sleep periods or \code{apply_troiano}/\code{apply_choi} to detect non-wear periods. It might be useful to combine the epochs data with the periods data, so that each epoch is labeled according to which period it falls into, if any. Then we can easily slice the epochs data by sleep/non-sleep or wear/non-wear.
#' @param epochs A \code{tibble} (\code{tbl}) of activity data with (at least) a timestamp column. The epoch length must be 60 seconds.
#' @param periods A summary \code{tibble} of the (sleep or non-wear) periods.
#' @param start_var The \code{periods} column which specifies when the periods start.
#' @param end_var The \code{periods} column which specifies when the periods end.
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
#'                                      start_var = "in_bed_timestamp",
#'                                      end_var = "out_bed_timestamp")
#'
#' # How many sleep periods were detected and what is their duration, in minutes?
#' periods_sleep$time_in_bed
#' # What is the assignment of epochs to periods?
#' table(agdb_sleep$period_id)
#' @export
combine_epochs_periods <- function(epochs, periods, start_var, end_var) {

  stopifnot(inherits(epochs, "tbl_agd"))
  stopifnot(inherits(periods, "tbl_period"))
  stopifnot(attr(epochs, "epochlength") == 60)
  stopifnot(exists(start_var, where = periods))
  stopifnot(exists(end_var, where = periods))

  epochs_periods <- epochs %>%
    select(- matches("period_id")) %>%
    left_join(expand_periods(periods, start_var, end_var),
              by = "timestamp")

  class(epochs_periods) <- class(epochs)
  for (a in setdiff(names(attributes(epochs)),
                    special_dplyr_attributes())) {
    attr(epochs_periods, a) <- attr(epochs, a)
  }
  epochs_periods
}
