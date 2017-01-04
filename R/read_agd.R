#' Read an *.agd file
#'
#' Read ActiGraph sleep watch data from a database stored in an AGD file. Return a tibble.
#' @param file Full path to an agd file to read.
#' @param tz Time zone to convert DateTime ticks to POSIX time.
#' @return A \code{tibble} (\code{tbl}) of activity data with at least two columns: timestamp and axis1 counts. Optional columns include axis2, axis2, steps, lux and inclinometer indicators (incline off, standing, sitting and lying). The device settings are stored as attributes, which include \code{epochlength}.
#' @references ActiLife 6 User's Manual by the ActiGraph Software Department. 04/03/2012.
#' @references \code{covertagd}: R package for converting agd files from ActiGraph into data.frames.
#' @seealso \code{\link{read_agd_raw}}
#' @examples
#' file <- system.file("extdata", "GT3XPlus-RawData-Day01-10sec.agd",
#'                     package = "actigraph.sleepr")
#' agdb <- read_agd(file)
#' agdb
#'
#' library("readr")
#' library("tools")
#'
#' # Read ActiGraph sleep watch data from the AGD files in a directory.
#' # Write the raw activity data to csv files in the user's home directory.
#' path_in <- system.file("extdata", package = "actigraph.sleepr")
#' path_out <- path.expand("~")
#' filenames <- list.files(path_in, pattern = ".agd", full.names = FALSE)
#' basenames <- file_path_sans_ext(filenames)
#'
#' for (basename in basenames) {
#'   file_in <- file.path(path_in, paste0(basename, ".agd"))
#'   file_out <- file.path(path_out, paste0(basename, ".csv"))
#'   agdb <- read_agd(file_in)
#'   write_csv(agdb, file_out)
#' }
#' @export

read_agd <- function(file, tz = "UTC") {

  ticks_to_dttm <- function(ticks, tz) {
    as.POSIXct(as.numeric(ticks) / 1e7,
               origin = "0001-01-01 00:00:00", tz)
  }

  agdb <- read_agd_raw(file, tz)
  data <- agdb$data %>%
    setNames(tolower(names(.))) %>%
    rename(timestamp = datatimestamp) %>%
    mutate_if(is.numeric, as.integer)

  # The settings are stored in a table with settingName, settingValue
  # columns and so all settings are of type `character`, including the
  # timestamps. I typecast the most salient settings appropriately.
  settings <- agdb$settings %>%
    setNames(tolower(names(.))) %>%
    select(settingname, settingvalue) %>%
    spread(settingname, settingvalue) %>%
    mutate_each(funs(ticks_to_dttm(., tz)), ends_with("time")) %>%
    mutate_each(funs(as.integer), starts_with("epoch"))

  tbl_agd(data, settings)
}

#' Read an *.agd file, with no post-processing
#'
#' Read ActiGraph sleep watch data from an SQLite database stored in an AGD file and return a list with (at least) five tables: data, sleep, filters, settings, awakenings. The tables have the schema described in the ActiLife 6 User manual and the timestamps are converted from Unix time format to human-readable POSIXct representation.
#' @param file Full path to an agd file to read.
#' @param tz Time zone to convert DateTime ticks to POSIX time.
#' @return A list of five tables: settings, data, filters, sleep, awakenings and, if available, capsense.
#' @details
#' Some Actigraph devices contain a capacitive sensor to detect monitor removal when worn against the skin. If that data is available, the return list includes a capsense table as well.
#' @references ActiLife 6 User's Manual by the ActiGraph Software Department. 04/03/2012.
#' @references \code{covertagd}: R package for converting agd files from ActiGraph into data.frames.
#' @seealso \code{\link{read_agd}}
#' @examples
#' file <- system.file("extdata", "GT3XPlus-RawData-Day01-10sec.agd",
#'                     package = "actigraph.sleepr")
#' agdb <- read_agd_raw(file)
#' str(agdb)
#' @export

read_agd_raw <- function(file, tz = "UTC") {

  stopifnot(file.exists(file))

  # Connect to the *.agd database
  db <- src_sqlite(file)

  # Get the names of all tables in the database
  query <- "SELECT name FROM sqlite_master WHERE type = 'table'"
  tables_agd <- db %>% tbl(sql(query)) %>% collect() %>% .$name
  tables_required <- c("data", "sleep", "awakenings", "filters", "settings")
  stopifnot(all(tables_required %in% tables_agd))

  # Cast Unix time ticks to POSIXct date/time
  # Timestamps are stored as ticks since 12:00:00 midnight, January 1, 0001.
  # Each tick is one ten-millionth of a second and so ticks are of type INTEGER
  # (long int). R does not have a 64 bit integer type and timestamps overflow.
  # So divide by 10,000,000 and convert to date/time with STRFTIME, before
  # selecting these columns from the database.
  cast_dttms <- function(x) {
    if (length(x)) ymd_hms(x, tz = tz) else as.POSIXct(x)
  }
  select_dttms <- function(table, vars) {
    query <- "SELECT"
    for (var in vars) {
      # Start counting seconds from January 1st, 1970;
      # 62135596800 is the number of seconds elapsed
      # from 01/01/0001 00:00:00 to 01/01/1970 00:00:00
      query <- paste0(query,
                      " STRFTIME('%Y-%m-%d %H:%M:%S', ",
                      var, "/", "10000000 - 62135596800, ",
                      "'unixepoch') AS ", var, "_ts, ")
    }
    query <- paste0(query, " * FROM ", table)
    db %>% tbl(sql(query)) %>%
      collect(n = Inf) %>%
      select(- one_of(vars)) %>%
      rename_(.dots = setNames(paste0(vars, "_ts"), vars)) %>%
      mutate_each(funs(cast_dttms), one_of(vars))
  }

  data <- select_dttms("data", "dataTimestamp")
  sleep <- select_dttms("sleep", c("inBedTimestamp",
                                   "outBedTimestamp"))
  awakenings <- select_dttms("awakenings", "timestamp")
  filters <- select_dttms("filters", c("filterStartTimestamp",
                                       "filterStopTimestamp"))
  settings <- db %>% tbl("settings") %>% collect()

  # The capsense table stores data from an optional wear sensor,
  # so it might not be present in the database.

  if ("capsense" %in% tables_agd)
    capsense <- select_dttms("capsense", "timeStamp")
  else
    capsense <- NULL

  return(list(data = data, sleep = sleep, filters = filters,
              settings = settings, awakenings = awakenings,
              capsense = capsense))
}
