#' Impute missing count values
#'
#' Trim leading and trailing NAs. Fill in the rest of the NAs using cubic spline interpolation.
#' @import zoo
#' @import lazyeval
#' @param agdb A \code{tibble} (\code{tbl}) of activity data (at least) an \code{epochlength} attribute.
#' @param activity_var The activity variable (e.g., axis1).
#' @return A \code{tibble} (\code{tbl}) of activity data with missing values in the \code{activity_var} column imputed.
#' @seealso \code{\link[zoo]{na.spline}}, \code{\link[zoo]{na.trim}}
#' @examples
#' file <- system.file("extdata", "GT3XPlus-RawData-Day01-10sec.agd",
#'                     package = "actigraph.sleepr")
#' agdb <- read_agd(file)
#' agdb$axis1[5:10] <- NA
#' agdb_imputed <- impute_na_epochs(agdb, "axis1")
#' agdb_imputed
#' @export
impute_na_epochs <- function(agdb, activity_var) {

  stopifnot(inherits(agdb, "tbl_agd"))
  agdb <- agdb %>% do(impute_na_epochs_(., activity_var))
}
impute_na_epochs_ <- function(data, activity_var) {

  spline_call <- interp(~ pmax(round(na.spline(var)), 0),
                        var = as.name(activity_var))

  data %>%
    # Don't impute at the start/the end of the time series, trim instead
    inner_join(na.trim(data %>% select_("timestamp", activity_var)),
               by = c("timestamp", activity_var)) %>%
    mutate_(.dots = setNames(list(spline_call), activity_var))
}
missing_epochs <- function(agdb) {

  stopifnot("tbl_agd" %in% class(agdb))
  if (anyNA(agdb$timestamp)) return(TRUE)

  epoch_len <- attr(agdb, "epochlength")
  any(agdb %>% do(missing_epochs_(., epoch_len)) %>% .$nas)
}
missing_epochs_ <- function(data, epoch_len) {

  epochs <- seq(first(data$timestamp),
                last(data$timestamp), by = epoch_len)
  data_frame(nas = !identical(data$timestamp, epochs))
}
