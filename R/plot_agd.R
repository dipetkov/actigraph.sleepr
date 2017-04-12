#' Plot activity values
#'
#' Plot a time series of activity values (by default, the counts on the vertical axis \emph{axis1}).
#' @param agdb A \code{tibble} (\code{tbl}) of activity data (at least) an \code{epochlength} attribute.
#' @param var The activity variable to plot. The default is axis1.
#' @param color Activity line color.
#' @param nrow,ncol Number of rows and columns. Relevant only if the activity data is grouped.
#' @examples
#' library("dplyr")
#' data("gtxplus1day")
#' data <- gtxplus1day %>%
#'   collapse_epochs(60) %>%
#'   apply_cole_kripke()
#'
#' plot_activity(data, color = "gray")
#' plot_activity(data, color = "sleep")
#' @export
plot_activity <- function(agdb, var = "axis1", color = "black",
                          nrow = NULL, ncol = NULL) {
  selected_vars <- intersect(names(agdb), color)
  if (length(selected_vars)) is.var <- TRUE else is.var <- FALSE
  if (is.var) {
    p <- ggplot(agdb, aes_string("timestamp", var,
                                 color = color, fill = color)) +
      geom_bar(stat = "identity")
  } else {
    p <- ggplot(agdb, aes_string("timestamp", var)) +
      geom_bar(stat = "identity", color = color, fill = color)
  }
  if (is.grouped_df(agdb)) {
    p <- p +
      facet_wrap(as.character(groups(agdb)),
                 nrow = nrow, ncol = ncol,
                 scales = "free_x")
  }
  p + theme_light() + labs(x = "time", y = var) + guides(fill = "none")
}
#' Plot activity and periods
#'
#' Plot activity values as a time series and periods as polygons.
#' @inheritParams plot_activity
#' @param periods A \code{tibble} of periods with at least two columns \code{start_var} and \code{end_var}.
#' @param act_var The activity variable to plot. The default is axis1.
#' @param start_var The column which indicates when the time periods start.
#' @param end_var The column which indicates when the time periods end.
#' @param fill Polygon fill color.
#' @examples
#' library("dplyr")
#' library("lubridate")
#' data("gtxplus1day")
#'
#' # Detect sleep periods using Sadeh as the sleep/awake algorithm
#' # and Tudor-Locke as the sleep period algorithm
#' periods_sleep <- gtxplus1day %>%
#'   collapse_epochs(60) %>%
#'   apply_cole_kripke() %>%
#'   apply_tudor_locke(min_sleep_period = 60)
#'
#' plot_activity_period(gtxplus1day, periods_sleep, "axis1",
#'                      "in_bed_time", "out_bed_time")
#' @export
plot_activity_period <- function(agdb, periods, act_var,
                                 start_var, end_var,
                                 color = "black", fill = "#525252",
                                 ncol = NULL, nrow = NULL) {
  stopifnot(identical(groups(agdb), groups(periods)))
  p <- plot_activity(agdb, var = act_var, color = color,
                     nrow = nrow, ncol = ncol)
  for (r in seq_len(nrow(periods))) {
    xmin <- periods$in_bed_time[r]
    xmax <- periods$out_bed_time[r]
    p <- p +
      annotate("rect",
               xmin = xmin, xmax = xmax, ymin = 0, ymax = Inf,
               fill = fill, alpha = 0.2)
  }
  p
}
