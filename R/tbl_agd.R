#' A \code{tibble} (\code{tbl}) of activity data exported by an ActiGraph device
#'
#' This tbl has several attributes, most importantly - \code{epochlength}.
#' @param data A data frame of raw activity counts.
#' @param settings A data frame of device settings.
#' @export
tbl_agd <- function(data, settings) {
  if (!is.data.frame(data))
    stop("`data` must be a data frame")
  if (!is.data.frame(settings))
    stop("`settings` must be a data frame")
  if (is.null(data$axis1))
    stop("`data` must have an `axis1` column")
  if (is.null(data$timestamp))
    stop("`data` must have a `timestamp` column")
  if (is.null(settings$epochlength))
    stop("`settings` must specify `epochlength`")
  data <- as_data_frame(data)
  for (key in names(settings)) {
    attr(data, key) <- settings[[key]]
  }
  structure(data, class = c("tbl_agd", "tbl_df", "tbl", "data.frame"))
}
#' A \code{tibble} (\code{tbl}) of automatically detected periods (of sleep or non-wear)
#'
#' This tbl has several attributes which specify the detection algorithm and the its parameter settings.
#' @param data A data frame of sleep or non-wear periods
#' @export
tbl_period <- function(data) {
  if (!is.data.frame(data))
    stop("`data` must be a data frame")
  if (is.null(data$axis1))
    stop("`data` must have an `in_bed_timestamp` column")
  if (is.null(data$timestamp))
    stop("`data` must have an `out_bed_timestamp` column")
  data <- as_data_frame(data)
  structure(data, class = c("tbl_period", "tbl_df", "tbl", "data.frame"))
}

special_dplyr_attributes <- function() {
  c("names", "class", "row.names", "vars", "drop", "indices", "labels",
    "group_sizes", "biggest_group_size")
}
#' @export
mutate_.tbl_agd <- function(x, ...) {
  y <- x
  class(y) <- class(y)[-1]
  y <- dplyr::mutate_(y, ...)
  for (a in setdiff(names(attributes(x)), special_dplyr_attributes())) {
    attr(y, a) <- attr(x, a)
  }
  class(y) <- c("tbl_agd", class(y))
  y
}
#' @export
rename_.tbl_agd <- function(x, ...) {
  y <- x
  class(y) <- class(y)[-1]
  y <- dplyr::rename_(y, ...)
  for (a in setdiff(names(attributes(x)), special_dplyr_attributes())) {
    attr(y, a) <- attr(x, a)
  }
  class(y) <- c("tbl_agd", class(y))
  y
}
#' @export
select_.tbl_agd <- function(x, ...) {
  y <- x
  class(y) <- class(y)[-1]
  y <- dplyr::select_(y, ...)
  for (a in setdiff(names(attributes(x)), special_dplyr_attributes())) {
    attr(y, a) <- attr(x, a)
  }
  class(y) <- c("tbl_agd", class(y))
  y
}
#' @export
filter_.tbl_agd <- function(x, ...) {
  y <- x
  class(y) <- class(y)[-1]
  y <- dplyr::filter_(y, ...)
  for (a in setdiff(names(attributes(x)), special_dplyr_attributes())) {
    attr(y, a) <- attr(x, a)
  }
  class(y) <- c("tbl_agd", class(y))
  y
}
#' @export
summarise_.tbl_agd <- function(x, ...) {
  y <- x
  class(y) <- class(y)[-1]
  y <- dplyr::summarise_(y, ...)
  for (a in setdiff(names(attributes(x)), special_dplyr_attributes())) {
    attr(y, a) <- attr(x, a)
  }
  class(y) <- c("tbl_agd", class(y))
  y
}
#' @export
group_by_.tbl_agd <- function(x, ...) {
  y <- x
  class(y) <- class(y)[-1]
  y <- dplyr::group_by_(y, ...)
  for (a in setdiff(names(attributes(x)), special_dplyr_attributes())) {
    attr(y, a) <- attr(x, a)
  }
  class(y) <- c("tbl_agd", class(y))
  y
}
#' @export
ungroup.tbl_agd <- function(x, ...) {
  y <- x
  class(y) <- class(y)[-1]
  y <- dplyr::ungroup(y, ...)
  for (a in setdiff(names(attributes(x)), special_dplyr_attributes())) {
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
  for (a in setdiff(names(attributes(x)), special_dplyr_attributes())) {
    attr(y, a) <- attr(x, a)
  }
  class(y) <- c("tbl_agd", class(y))
  y
}
#' @export
mutate_.tbl_period <- function(x, ...) {
  y <- x
  class(y) <- class(y)[-1]
  y <- dplyr::mutate_(y, ...)
  for (a in setdiff(names(attributes(x)), special_dplyr_attributes())) {
    attr(y, a) <- attr(x, a)
  }
  class(y) <- c("tbl_period", class(y))
  y
}
#' @export
rename_.tbl_period <- function(x, ...) {
  y <- x
  class(y) <- class(y)[-1]
  y <- dplyr::rename_(y, ...)
  for (a in setdiff(names(attributes(x)), special_dplyr_attributes())) {
    attr(y, a) <- attr(x, a)
  }
  class(y) <- c("tbl_period", class(y))
  y
}
#' @export
select_.tbl_period <- function(x, ...) {
  y <- x
  class(y) <- class(y)[-1]
  y <- dplyr::select_(y, ...)
  for (a in setdiff(names(attributes(x)), special_dplyr_attributes())) {
    attr(y, a) <- attr(x, a)
  }
  class(y) <- c("tbl_period", class(y))
  y
}
#' @export
filter_.tbl_period <- function(x, ...) {
  y <- x
  class(y) <- class(y)[-1]
  y <- dplyr::filter_(y, ...)
  for (a in setdiff(names(attributes(x)), special_dplyr_attributes())) {
    attr(y, a) <- attr(x, a)
  }
  class(y) <- c("tbl_period", class(y))
  y
}
#' @export
summarise_.tbl_period <- function(x, ...) {
  y <- x
  class(y) <- class(y)[-1]
  y <- dplyr::summarise_(y, ...)
  for (a in setdiff(names(attributes(x)), special_dplyr_attributes())) {
    attr(y, a) <- attr(x, a)
  }
  class(y) <- c("tbl_period", class(y))
  y
}
#' @export
group_by_.tbl_period <- function(x, ...) {
  y <- x
  class(y) <- class(y)[-1]
  y <- dplyr::group_by_(y, ...)
  for (a in setdiff(names(attributes(x)), special_dplyr_attributes())) {
    attr(y, a) <- attr(x, a)
  }
  class(y) <- c("tbl_period", class(y))
  y
}
#' @export
ungroup.tbl_period <- function(x, ...) {
  y <- x
  class(y) <- class(y)[-1]
  y <- dplyr::ungroup(y, ...)
  for (a in setdiff(names(attributes(x)), special_dplyr_attributes())) {
    attr(y, a) <- attr(x, a)
  }
  class(y) <- c("tbl_period", class(y))
  y
}
#' @export
do_.tbl_period <- function(x, ...) {
  y <- x
  class(y) <- class(y)[-1]
  y <- dplyr::do_(y, ...)
  for (a in setdiff(names(attributes(x)), special_dplyr_attributes())) {
    attr(y, a) <- attr(x, a)
  }
  class(y) <- c("tbl_period", class(y))
  y
}
