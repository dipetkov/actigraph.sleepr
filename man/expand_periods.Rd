% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{expand_periods}
\alias{expand_periods}
\title{Expand time periods into a data frame of equally spaced time points}
\usage{
expand_periods(periods, start_var, end_var, units = "1 min")
}
\arguments{
\item{periods}{A data frame with at least two columns,
\code{start_var} and \code{end_var} which are the first
and the last epoch in a set of time periods, e.g. sleep
periods or (non)wear periods.}

\item{start_var}{The variable (unquoted) which indicates
when the time periods start.}

\item{end_var}{The variable (unquoted) which indicates when
the time periods end.}

\item{units}{The time unit as a characters string. The default is \code{"1 min"}.}
}
\description{
Expand time periods into a data frame of equally spaced time points
}
\examples{
library("dplyr")
data("gtxplus1day")

gtxplus1day \%>\%
  collapse_epochs(60) \%>\%
  apply_choi(min_period_len = 45) \%>\%
  expand_periods(period_start, period_end, units = "30 mins")
}
