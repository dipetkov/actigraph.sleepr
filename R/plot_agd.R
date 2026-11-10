#' Plot activity values
#'
#' Plot a time series of activity values (by default, the counts on the
#' vertical axis *axis1*).
#' @param agdb A `tibble` of activity data.
#' @param var The activity variable (unquoted) to plot on the y-axis.
#' @param color Activity line color.
#' @param nrow,ncol Number of rows and columns. Relevant only if the
#' activity data is grouped.
#' @return A `ggplot` object. It shows the activity values in column `var`
#' as a time series, faceted by group when the input `agdb` is grouped.
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' data("gtxplus1day")
#' sub_gt3x <- gtxplus1day |>
#'   dplyr::filter(timestamp <= lubridate::as_datetime("2012-06-27 18:00:00"))
#'
#' data <- sub_gt3x |>
#'   collapse_epochs(60) |>
#'   apply_cole_kripke()
#'
#' plot_activity(data, axis1, color = "gray")
#' plot_activity(data, axis1, color = "sleep")
#' @export
plot_activity <- function(agdb, var, color = "black",
                          nrow = NULL, ncol = NULL) {
  rlang::check_installed("ggplot2", reason = "to plot activity data.")
  var <- enquo(var)
  if (color %in% names(agdb)) {
    p <- agdb |>
      ggplot2::ggplot(
        ggplot2::aes(timestamp, !!var, fill = !!ensym(color))
      ) +
      ggplot2::geom_col()
  } else {
    p <- agdb |>
      ggplot2::ggplot(
        ggplot2::aes(timestamp, !!var)
      ) +
      ggplot2::geom_col(
        fill = color
      )
  }
  if (is.grouped_df(agdb)) {
    p <- p +
      ggplot2::facet_wrap(
        group_vars(agdb),
        nrow = nrow, ncol = ncol,
        scales = "free_x"
      )
  }
  p + ggplot2::theme_light() + ggplot2::labs(x = "time")
}

#' Plot activity and periods
#'
#' Plot activity values as a time series and periods as polygons.
#' @inheritParams plot_activity
#' @param periods A `tibble` of periods with at least two columns
#' `start_var` and `end_var`.
#' @param act_var The activity variable (unquoted) to plot on the y-axis.
#' @param start_var The variable (unquoted) which indicates when the
#' time periods start.
#' @param end_var The variable (unquoted) which indicates when the time
#'  periods end.
#' @param fill Polygon fill color.
#' @return A `ggplot` object. It shows the activity values as a time series
#' and each period as a rectangle from `start_var` to `end_var`.
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' data("gtxplus1day")
#'
#' sub_gt3x <- gtxplus1day |>
#'   dplyr::filter(timestamp <= lubridate::as_datetime("2012-06-27 18:00:00"))
#' # Detect sleep periods using Sadeh as the sleep/awake algorithm
#' # and Tudor-Locke as the sleep period algorithm
#' periods_sleep <- sub_gt3x |>
#'   collapse_epochs(60) |>
#'   apply_cole_kripke() |>
#'   apply_tudor_locke(min_sleep_period = 60)
#'
#' plot_activity_period(
#'   sub_gt3x, periods_sleep, axis1,
#'   in_bed_time, out_bed_time
#' )
#' @export
plot_activity_period <- function(agdb, periods, act_var,
                                 start_var, end_var,
                                 color = "black", fill = "#525252",
                                 ncol = NULL, nrow = NULL) {
  rlang::check_installed("ggplot2", reason = "to plot activity data.")
  act_var <- enquo(act_var)
  start_var <- enquo(start_var)
  end_var <- enquo(end_var)
  p <-
    plot_activity(
      agdb, !!act_var,
      color = color, nrow = nrow, ncol = ncol
    )
  p +
    ggplot2::geom_rect(
      data = periods,
      ggplot2::aes(
        xmin = !!start_var, ymin = 0,
        xmax = !!end_var, ymax = Inf
      ),
      inherit.aes = FALSE,
      fill = fill, alpha = 0.2
    )
}
