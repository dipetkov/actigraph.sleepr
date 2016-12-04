#' Re-integrate epochs
#'
#' Collapse post-filtered activity counts into larger epoch "buckets".
#' @import dplyr
#' @param agdb A \code{tibble} (\code{tbl}) of activity data (at least) an \code{epochlength} attribute.
#' @param epoch_len_out Output (longer) epoch length in seconds, must be exact multiple of the input epoch length. Currently only \code{epoch_len_out} = 60 is supported.
#' @param use_incomplete logical. Set to \code{TRUE} to follow ActiLife convention, which collapses all observed epochs even if they are incomplete.
#' @return A \code{tibble} (\code{tbl}) of activity data collapsed into one-minute epochs.
#' @references ActiLife 6 User's Manual by the ActiGraph Software Department. 04/03/2012.
#' @details
#' Activity counts cannot be reintegrated into shorter epochs, e.g., 60s -> 10s. Currently, \code{collapse_epochs} integrates into 60s epochs only. This is not general but is sufficient for sleep analysis because the standard Sadeh and Cole-Kripke sleep algorithms were developed for 60s epoch data.
#'
#' Suppose we want to collapse from 15 to 60 seconds. A complete 60s epoch consists of four 15s epochs: 00, 15, 45 and 60. However, the first and last epochs would be incomplete if the device started/stopped collecting data mid-minute. ActiLife 6 uses these epochs anyway. For example, if only 45 and 60 are available for the first minute, then ActiLife will aggregate across these two epochs only. This is a reasonable approach to sleep analysis with the Sadeh and the Cole-Kripke algorithms which pad the beginning and the end of the time series with zeros anyway.
#' @examples
#' file <- system.file("extdata", "GT3XPlus-RawData-Day01-10sec.agd",
#'                     package = "actigraph.sleepr")
#' agdb_10s <- read_agd(file)
#' agdb_60s <- collapse_epochs(agdb_10s, 60)
#' @export

collapse_epochs <- function(agdb, epoch_len_out,
                            use_incomplete = TRUE) {

  stopifnot(inherits(agdb, "tbl_agd"))
  epoch_len_in <- attr(agdb, "epochlength")

  if (epoch_len_out != 60)
    stop("Use `collapse_epochs` to aggregate to 60s epochs.")
  if (epoch_len_out %% epoch_len_in)
    stop("Output epoch length is not an exact multiple ",
         "of input epoch length.")

  collapse_factor <- epoch_len_out / epoch_len_in
  if (collapse_factor == 1) return(agdb)

  if (missing_epochs(agdb))
    stop("Missing timestamps. ",
         "Epochs should be evenly spaced from ",
         "first(timestamp) to last(timestamp).")
  if (anyNA(agdb$axis1))
    stop("Missing axis1 counts. ",
         "These can be imputed with `impute_na_epochs`.")

  # TODO: a more general approach to collapsing
  # might use the findInterval function
  # though care must be taken with "incomplete"
  # epochs at the start/end of the time series

  agdb <- agdb %>%
    do(collapse_epochs_(., collapse_factor, use_incomplete))

  attr(agdb, "epochlength") <- epoch_len_out
  attr(agdb, "epochcount") <- nrow(agdb)
  agdb
}

collapse_epochs_ <- function(data, collapse_factor, use_incomplete) {

  na_incomplete <- function(x, n) {
    if (use_incomplete) x else ifelse(n == collapse_factor, x, NA)
  }
  truncate_seconds <- function(x) {
    # `trunc` truncates the timestamps up to seconds but returns POSIXlt
    # `dplyr` doesn't support POSIXlt, so convert to POSIXct
    as.POSIXct(trunc(x, units = "mins"))
  }

  # I've excluded lux because to aggregate lux correctly,
  # it's necessary to take floor(mean(lux)) instead of sum(lux)
  vars <- intersect(colnames(data),
                    c("axis1", "axis2", "axis3", "steps",
                      "inclineoff", "inclinestanding",
                      "inclinesitting", "inclinelying"))

  data %>%
    mutate(timestamp = truncate_seconds(timestamp), n = 1L) %>%
    group_by(timestamp) %>%
    summarise_each(funs(sum), one_of(c(vars, "n"))) %>%
    mutate_each(funs(na_incomplete(., n)), - timestamp, - n) %>%
    select(- n)
}
