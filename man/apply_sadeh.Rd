% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/apply_sadeh.R
\name{apply_sadeh}
\alias{apply_sadeh}
\title{Apply the Sadeh algorithm}
\usage{
apply_sadeh(agdb)
}
\arguments{
\item{agdb}{A \code{tibble} of activity data with an \code{epochlength} attribute.
The epoch length must be 60 seconds.}
}
\value{
A \code{tibble} of activity data. A new column \code{sleep} indicates whether
each 60s epoch is scored as asleep (S) or awake (W).
}
\description{
The Sadeh sleep scoring algorithm is primarily used for younger
adolescents as the supporting research was performed on children
and young adults.
}
\details{
The Sadeh algorithm requires that the activity data is in 60s
epochs and uses an 11-minute window that includes the five previous
and five future epochs. This function implements the algorithm as
described in the ActiGraph user manual.

The Sadeh algorithm uses the y-axis (axis 1) counts; epoch counts
over 300 are set to 300. The sleep index (SI) is defined as\preformatted{SI = 7.601 - (0.065 * AVG) - (1.08 * NATS) - (0.056 * SD) - (0.703 * LG)
}

where at epoch \code{t}

\describe{
\item{AVG}{the arithmetic mean (average) of the activity counts in
an 11-epoch window centered at \code{t}}
\item{NATS}{the number of epochs in this 11-epoch window which
have counts >= 50 and < 100}
\item{SD}{the standard deviation of the counts in a 6-epoch
window that includes \code{t} and the five preceding epochs}
\item{LG}{the natural (base e) logarithm of the activity at
epoch \code{t}. To avoid taking the log of 0, we add 1 to the count.}
}

The time series of activity counts is padded with zeros as
necessary, at the beginning and at the end, to compute the three
functions AVG, SD, NATS within a rolling window.

Finally, the sleep state is asleep (S) if the sleep index SI
is greater than -4; otherwise the sleep state is awake (W).
}
\examples{
library("dplyr")
data("gtxplus1day")

gtxplus1day \%>\%
  collapse_epochs(60) \%>\%
  apply_sadeh()
}
\references{
A Sadeh, KM Sharkey and MA Carskadon. Activity
based sleep-wake identification: An empirical test of methodological
issues. \emph{Sleep}, 17(3):201–207, 1994.

ActiLife 6 User's Manual by the ActiGraph Software
Department. 04/03/2012.
}
\seealso{
\code{\link[=apply_cole_kripke]{apply_cole_kripke()}}, \code{\link[=apply_cole_kripke]{apply_cole_kripke()}}, \code{\link[=apply_tudor_locke]{apply_tudor_locke()}}
}
