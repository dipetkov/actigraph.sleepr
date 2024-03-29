% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_agd.R
\name{read_agd}
\alias{read_agd}
\title{Read activity counts from an *.agd file}
\usage{
read_agd(file, tz = "UTC")
}
\arguments{
\item{file}{Full path to an agd file to read.}

\item{tz}{Time zone to convert DateTime ticks to POSIX time.}
}
\value{
A \code{tibble} of activity data with at
least two columns: timestamp and axis1 counts. Optional columns
include axis2, axis2, steps, lux and inclinometer indicators
(incline off, standing, sitting and lying). The device settings
are stored as attributes, which include \code{epochlength}.
}
\description{
Read ActiGraph sleep watch data from a database stored in an
AGD file. Return a tibble.
}
\examples{
file <- system.file("extdata", "GT3XPlus-RawData-Day01.agd",
  package = "actigraph.sleepr"
)
read_agd(file)

library("dplyr")
library("purrr")

# Read ActiGraph sleep watch data from the AGD files in a directory
# and bind the data into one data frame indexed by `.filename`.
path <- system.file("extdata", package = "actigraph.sleepr")

list.files(path, pattern = "*.agd", full.names = TRUE) \%>\%
  map_dfr(read_agd, .id = ".filename")
}
\references{
The AGD file format is described in the ActiLife 6 Manual.
\url{https://actigraphcorp.com/support/manuals/actilife-6-manual/}
}
\seealso{
\code{\link[=read_agd_raw]{read_agd_raw()}}
}
