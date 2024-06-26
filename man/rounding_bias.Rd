% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rounding-bias.R
\name{rounding_bias}
\alias{rounding_bias}
\title{Compute rounding bias}
\usage{
rounding_bias(
  x,
  digits,
  rounding = "up",
  threshold = 5,
  symmetric = FALSE,
  mean = TRUE
)
}
\arguments{
\item{x}{Numeric or string coercible to numeric.}

\item{digits}{Integer. Number of decimal digits to which \code{x} will be rounded.}

\item{rounding}{String. Rounding procedure that will be applied to \code{x}. See
\code{vignette("rounding-options")}. Default is \code{"up"}.}

\item{threshold, symmetric}{Further arguments passed down to \code{\link[=reround]{reround()}}.}

\item{mean}{Logical. If \code{TRUE} (the default), the mean total of bias will be
returned. Set \code{mean} to \code{FALSE} to get a vector of individual biases the
length of \code{x}.}
}
\value{
Numeric. By default of \code{mean}, the length is 1; otherwise, it is the
same length as \code{x}.
}
\description{
Rounding often leads to bias, such that the mean of a rounded
distribution is different from the mean of the original distribution. Call
\code{rounding_bias()} to compute the amount of this bias.
}
\details{
Bias is calculated by subtracting the original vector, \code{x}, from a
vector rounded in the specified way.

The function passes all arguments except for \code{mean} down to \code{\link[=reround]{reround()}}.
Other than there, however, \code{rounding} is \code{"up"} by default, and it can't be
set to \code{"up_or_down"}, \code{"up_from_or_down_from"}, or\code{"ceiling_or_floor"}.
}
\examples{
# Define example vector:
vec <- seq(from = 0.01, to = 0.1, by = 0.01)
vec

# The default rounds `x` up from 5:
rounding_bias(x = vec, digits = 1)

# Other rounding procedures are supported,
# such as rounding down from 5...
rounding_bias(x = vec, digits = 1, rounding = "down")

# ...or rounding to even with `base::round()`:
rounding_bias(x = vec, digits = 1, rounding = "even")
}
