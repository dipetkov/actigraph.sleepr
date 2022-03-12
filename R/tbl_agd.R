#' A `tibble` of activity data exported by an ActiGraph device
#'
#' This tibble has several attributes, most importantly, `epochlength`.
#' @param data A data frame of raw activity counts.
#' @param settings A data frame of device settings.
#' @export
tbl_agd <- function(data, settings) {
  assert_that(
    is.data.frame(data),
    is.data.frame(settings),
    has_name(data, "axis1"),
    has_name(data, "timestamp"),
    has_name(settings, "epochlength")
  )

  for (key in names(settings)) {
    attr(data, key) <- settings[[key]]
  }
  structure(data, class = c("tbl_df", "tbl", "data.frame"))
}

add_magnitude <- function(data) {
  data %>%
    mutate(magnitude = sqrt(.data$axis1^2 + .data$axis2^2 + .data$axis3^2))
}
