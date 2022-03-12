#' Impute missing count values
#'
#' Trim leading and trailing NAs. Fill in the rest of the NAs using
#' cubic spline interpolation.
#' @param agdb A `tibble` of activity data with an `epochlength` attribute.
#' @param ... Comma separated list of unquoted variables.
#' @return A`tibble` of activity data. Each variable in `...` is imputed.
#' @seealso [zoo::na.spline()], [zoo::na.trim()]
#' @examples
#' library("dplyr")
#' data("gtxplus1day")
#'
#' gtxplus1day$axis1[5:10] <- NA
#' gtxplus1day %>%
#'   impute_epochs(axis1)
#' @export
impute_epochs <- function(agdb, ...) {
  selected <- tidyselect::vars_select(names(agdb), ...)
  if (length(selected) == 0) {
    return(agdb)
  }

  agdb %>%
    group_modify(
      ~ impute_epochs_(., selected)
    )
}
impute_epochs_ <- function(data, selected) {
  impute <- function(x) pmax(round(na.spline(x)), 0)

  data %>%
    select(timestamp, !!!selected) %>%
    na.trim() %>%
    inner_join(data,
      by = c("timestamp", selected)
    ) %>%
    mutate(
      across(all_of(selected), impute)
    )
}
#' Checks whether there are gaps in the time series
#'
#' The timestamps in the agd time series should run from `first(timestamp)` to
#' `last(timestamp)` in increments of `epochlength` seconds. This function
#' checks whether this holds or not. If the data is grouped (e.g., by subject),
#' the check is performed for each group separately.
#' @param agdb A `tibble` of activity data with an `epochlength` attribute.
#' @return `TRUE` or `FALSE`
#' @export
has_missing_epochs <- function(agdb) {
  if (anyNA(agdb$timestamp)) {
    return(TRUE)
  }

  agdb <- agdb %>%
    group_modify(
      ~ has_missing_epochs_(.)
    )
  any(agdb$missing)
}

has_missing_epochs_ <- function(data) {
  epoch_len <- get_epoch_length(data)

  epochs <- seq(first(data$timestamp), last(data$timestamp),
    by = epoch_len
  )
  tibble::tibble(missing = !identical(epochs, data$timestamp))
}
