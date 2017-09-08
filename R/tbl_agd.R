#' A \code{tibble} (\code{tbl}) of activity data exported by an ActiGraph device
#'
#' This tbl has several attributes, most importantly - \code{epochlength}.
#' @param data A data frame of raw activity counts.
#' @param settings A data frame of device settings.
#' @export
tbl_agd <- function(data, settings) {
  assert_that(is.data.frame(data))
  assert_that(is.data.frame(settings))
  assert_that(has_name(data, "axis1"))
  assert_that(has_name(data, "timestamp"))
  assert_that(has_name(settings, "epochlength"))
  for (key in names(settings)) {
    attr(data, key) <- settings[[key]]
  }
  structure(data, class = c("tbl_agd", "tbl_df", "tbl", "data.frame"))
}

special_dplyr_attributes <- function() {
  c("names", "class", "row.names", "vars", "drop", "indices", "labels",
    "group_sizes", "biggest_group_size")
}

add_magnitude <- function(data) {
  data %>%
    mutate(magnitude = sqrt(.data$axis1 ^ 2 + .data$axis2 ^ 2 +
                              .data$axis3 ^ 2))
}

wrap_dplyr_verb <- function(x, f, ...) {
  y <- x
  cls1 <- class(y)[1]
  class(y) <- class(y)[-1]
  y <- f(y, ...)
  for (a in setdiff(names(attributes(x)), special_dplyr_attributes())) {
    attr(y, a) <- attr(x, a)
  }
  class(y) <- c(cls1, class(y))
  y
}

wrap_dplyr_join <- function(x, y, f, ...) {
  cls1 <- class(y)[1]
  class(x) <- class(x)[-1]
  z <- f(x, y, ...)
  for (a in setdiff(names(attributes(x)), special_dplyr_attributes())) {
    attr(z, a) <- attr(x, a)
  }
  class(z) <- c(cls1, class(z))
  z
}

#' @export
mutate.tbl_agd <- function(x, ...) {
  wrap_dplyr_verb(x, dplyr::mutate, ...)
}
#' @export
rename.tbl_agd <- function(x, ...) {
  wrap_dplyr_verb(x, dplyr::rename, ...)
}
#' @export
select.tbl_agd <- function(x, ...) {
  wrap_dplyr_verb(x, dplyr::select, ...)
}
#' @export
filter.tbl_agd <- function(x, ...) {
  wrap_dplyr_verb(x, dplyr::filter, ...)
}
#' @export
summarise.tbl_agd <- function(x, ...) {
  wrap_dplyr_verb(x, dplyr::summarise, ...)
}
#' @export
group_by.tbl_agd <- function(x, ...) {
  wrap_dplyr_verb(x, dplyr::group_by, ...)
}
#' @export
ungroup.tbl_agd <- function(x, ...) {
  wrap_dplyr_verb(x, dplyr::ungroup, ...)
}
#' @export
do.tbl_agd <- function(x, ...) {
  wrap_dplyr_verb(x, dplyr::do, ...)
}
#' @export
arrange.tbl_agd <- function(x, ...) {
  wrap_dplyr_verb(x, dplyr::arrange, ...)
}
#' @export
inner_join.tbl_agd <- function(x, y, by = NULL, copy = FALSE,
                               suffix = c(".x", ".y"), ...) {
  wrap_dplyr_join(x, y, dplyr::inner_join, by = by, copy = copy,
                  suffix = suffix, ...)
}
#' @export
left_join.tbl_agd <- function(x, y, by = NULL, copy = FALSE,
                              suffix = c(".x", ".y"), ...) {
  wrap_dplyr_join(x, y, dplyr::left_join, by = by, copy = copy,
                  suffix = suffix, ...)
}
#' @export
right_join.tbl_agd <- function(x, y, by = NULL, copy = FALSE,
                               suffix = c(".x", ".y"), ...) {
  wrap_dplyr_join(x, y, dplyr::right_join, by = by, copy = copy,
                  suffix = suffix, ...)
}
#' @export
full_join.tbl_agd <- function(x, y, by = NULL, copy = FALSE,
                              suffix = c(".x", ".y"), ...) {
  wrap_dplyr_join(x, y, dplyr::full_join, by = by, copy = copy,
                  suffix = suffix, ...)
}
#' @export
semi_join.tbl_agd <- function(x, y, by = NULL, copy = FALSE, ...) {
  wrap_dplyr_join(x, y, dplyr::semi_join, by = by, copy = copy, ...)
}
#' @export
anti_join.tbl_agd <- function(x, y, by = NULL, copy = FALSE, ...) {
  wrap_dplyr_join(x, y, dplyr::anti_join, by = by, copy = copy, ...)
}
