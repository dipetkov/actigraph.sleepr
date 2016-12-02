#' A \code{tibble} (\code{tbl}) of activity data exported by an ActiGraph device. This tbl has several attributes, most importantly \code{epochlength}.
#' @param data A data frame of raw activity counts.
#' @param settings A data frame of device settings.
#' @export
tbl_agd <- function(data, settings) {
  if (!is.data.frame(data)) stop("`data` must be a data frame")
  if (!is.data.frame(settings)) stop("`settings` must be a data frame")
  data <- as_data_frame(data)
  for (key in names(settings)) {
    attr(data, key) <- settings[[key]]
  }
  structure(data, class = c("tbl_agd", "tbl_df", "tbl", "data.frame"))
}
#' @export
mutate_.tbl_agd <- function(x, ...) {
  y <- x
  class(y) <- class(y)[-1]
  y <- dplyr::mutate_(y, ...)
  for (a in setdiff(names(attributes(x)), c("names"))) {
    attr(y, a) <- attr(x, a)
  }
  y
}
#' @export
rename_.tbl_agd <- function(x, ...) {
  y <- x
  class(y) <- class(y)[-1]
  y <- dplyr::rename_(y, ...)
  for (a in setdiff(names(attributes(x)), c("names"))) {
    attr(y, a) <- attr(x, a)
  }
  y
}
#' @export
select_.tbl_agd <- function(x, ...) {
  y <- x
  class(y) <- class(y)[-1]
  y <- dplyr::select_(y, ...)
  for (a in setdiff(names(attributes(x)), c("names"))) {
    attr(y, a) <- attr(x, a)
  }
  y
}
#' @export
filter_.tbl_agd <- function(x, ...) {
  y <- x
  class(y) <- class(y)[-1]
  y <- dplyr::filter_(y, ...)
  for (a in setdiff(names(attributes(x)), c("row.names"))) {
    attr(y, a) <- attr(x, a)
  }
  y
}
#' @export
summarise_.tbl_agd <- function(x, ...) {
  y <- x
  class(y) <- class(y)[-1]
  y <- dplyr::summarise_(y, ...)
  for (a in setdiff(names(attributes(x)), c("names", "row.names"))) {
    attr(y, a) <- attr(x, a)
  }
  y
}
#' @export
group_by_.tbl_agd <- function(x, ...) {
  y <- x
  class(y) <- class(y)[-1]
  y <- dplyr::group_by_(y, ...)
  for (a in setdiff(names(attributes(x)), c("class"))) {
    attr(y, a) <- attr(x, a)
  }
  class(y) <- c("tbl_agd", class(y))
  y
}
#' @export
do_.tbl_agd <- function(x, ...) {
  y <- x
  class(y) <- class(y)[-1]
  y <- dplyr::do_(y, ...)
  for (a in setdiff(names(attributes(x)), c("class", "names", "row.names"))) {
    attr(y, a) <- attr(x, a)
  }
  class(y) <- c("tbl_agd", class(y))
  y
}
