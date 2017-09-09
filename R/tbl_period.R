#' A \code{tibble} (\code{tbl}) of automatically detected periods (of sleep or non-wear)
#'
#' This tbl has several attributes which specify the detection algorithm and the its parameter settings.
#' @param data A data frame of sleep or non-wear periods
#' @export
tbl_period <- function(data) {
  assert_that(is.data.frame(data))
  assert_that(has_name(data, "axis1"))
  assert_that(has_name(data, "timestamp"))
  structure(data, class = c("tbl_period", "tbl_df", "tbl", "data.frame"))
}
#' @export
mutate.tbl_period <- function(.data, ...) {
  wrap_dplyr_verb(.data, dplyr::mutate, ...)
}
#' @export
rename.tbl_period <- function(.data, ...) {
  wrap_dplyr_verb(.data, dplyr::rename, ...)
}
#' @export
select.tbl_period <- function(.data, ...) {
  wrap_dplyr_verb(.data, dplyr::select, ...)
}
#' @export
filter.tbl_period <- function(.data, ...) {
  wrap_dplyr_verb(.data, dplyr::filter, ...)
}
#' @export
summarise.tbl_period <- function(.data, ...) {
  wrap_dplyr_verb(.data, dplyr::summarise, ...)
}
#' @export
group_by.tbl_period <- function(.data, ...) {
  wrap_dplyr_verb(.data, dplyr::group_by, ...)
}
#' @export
ungroup.tbl_period <- function(x, ...) {
  wrap_dplyr_verb(x, dplyr::ungroup, ...)
}
#' @export
do.tbl_period <- function(.data, ...) {
  wrap_dplyr_verb(.data, dplyr::do, ...)
}
#' @export
arrange.tbl_period <- function(.data, ...) {
  wrap_dplyr_verb(.data, dplyr::arrange, ...)
}
#' @export
inner_join.tbl_period <- function(x, y, by = NULL, copy = FALSE,
                                  suffix = c(".x", ".y"), ...) {
  wrap_dplyr_join(x, y, dplyr::inner_join, by = by, copy = copy,
                  suffix = suffix, ...)
}
#' @export
left_join.tbl_period <- function(x, y, by = NULL, copy = FALSE,
                                 suffix = c(".x", ".y"), ...) {
  wrap_dplyr_join(x, y, dplyr::left_join, by = by, copy = copy,
                  suffix = suffix, ...)
}
#' @export
right_join.tbl_period <- function(x, y, by = NULL, copy = FALSE,
                                  suffix = c(".x", ".y"), ...) {
  wrap_dplyr_join(x, y, dplyr::right_join, by = by, copy = copy,
                  suffix = suffix, ...)
}
#' @export
full_join.tbl_period <- function(x, y, by = NULL, copy = FALSE,
                                 suffix = c(".x", ".y"), ...) {
  wrap_dplyr_join(x, y, dplyr::full_join, by = by, copy = copy,
                  suffix = suffix, ...)
}
#' @export
semi_join.tbl_period <- function(x, y, by = NULL, copy = FALSE, ...) {
  wrap_dplyr_join(x, y, dplyr::semi_join, by = by, copy = copy, ...)
}
#' @export
anti_join.tbl_period <- function(x, y, by = NULL, copy = FALSE, ...) {
  wrap_dplyr_join(x, y, dplyr::anti_join, by = by, copy = copy, ...)
}
