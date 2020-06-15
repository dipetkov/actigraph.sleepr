#' Plot activity values
#'
#' Plot a time series of activity values (by default, the counts on the
#' vertical axis \emph{axis1}).
#' @param agdb A \code{tibble} (\code{tbl}) of activity data (at least)
#' an \code{epochlength} attribute.
#' @param var The activity variable (unquoted) to plot on the y-axis.
#' @param color Activity line color.
#' @param nrow,ncol Number of rows and columns. Relevant only if the
#' activity data is grouped.
#' @examples
#' data("gtxplus1day")
#' sub_gt3x = gtxplus1day %>%
#'    dplyr::filter(timestamp <= lubridate::as_datetime("2012-06-27 18:00:00"))
#'
#' data <- sub_gt3x %>%
#'   collapse_epochs(60) %>%
#'   apply_cole_kripke()
#'
#' plot_activity(data, axis1, color = "gray")
#' plot_activity(data, axis1, color = "sleep")
#' @export
plot_activity <- function(agdb, var, color = "black",
                          nrow = NULL, ncol = NULL) {
  var <- enquo(var)
  if (color %in% names(agdb)) {
    p <- ggplot(agdb, aes_string("timestamp", quo_text(var),
                                 color = color, fill = color)) +
      geom_col()
  } else {
    p <- ggplot(agdb, aes_string("timestamp", quo_text(var))) +
      geom_col(color = color, fill = color)
  }
  if (is.grouped_df(agdb)) {
    p <- p +
      facet_wrap(as.character(groups(agdb)),
                 nrow = nrow, ncol = ncol,
                 scales = "free_x")
  }
  p + theme_light() + labs(x = "time")
}
#' Plot activity and periods
#'
#' Plot activity values as a time series and periods as polygons.
#' @inheritParams plot_activity
#' @param periods A \code{tibble} of periods with at least two columns
#' \code{start_var} and \code{end_var}.
#' @param act_var The activity variable (unquoted) to plot on the y-axis.
#' @param start_var The variable (unquoted) which indicates when the
#' time periods start.
#' @param end_var The variable (unquoted) which indicates when the time
#'  periods end.
#' @param fill Polygon fill color.
#' @examples
#' data("gtxplus1day")
#'
#' sub_gt3x = gtxplus1day %>%
#'    dplyr::filter(timestamp <= lubridate::as_datetime("2012-06-27 18:00:00"))
#' # Detect sleep periods using Sadeh as the sleep/awake algorithm
#' # and Tudor-Locke as the sleep period algorithm
#' periods_sleep <- sub_gt3x %>%
#'   collapse_epochs(60) %>%
#'   apply_cole_kripke() %>%
#'   apply_tudor_locke(min_sleep_period = 60)
#'
#' plot_activity_period(sub_gt3x, periods_sleep, axis1,
#'                      in_bed_time, out_bed_time)
#' @export
plot_activity_period <- function(agdb, periods, act_var,
                                 start_var, end_var,
                                 color = "black", fill = "#525252",
                                 ncol = NULL, nrow = NULL) {
  act_var <- enquo(act_var)
  start_var <- enquo(start_var)
  end_var <- enquo(end_var)
  plot_activity(agdb, !!act_var, color = color,
                nrow = nrow, ncol = ncol) +
    geom_rect(data = periods,
              aes_string(xmin = quo_text(start_var), ymin = 0,
                         xmax = quo_text(end_var), ymax = Inf),
              inherit.aes = FALSE,
              fill = fill, alpha = 0.2)
}
