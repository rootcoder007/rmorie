% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Comb}
\alias{Comb}
\title{H(z) = (1/2)(1 - z^-N): N zeros spaced evenly round the unit circle,}
\usage{
Comb(period_samples, fs = 1000, z = NULL)
}
\arguments{
\item{period_samples}{Coerced to integer by the body, with \code{as.integer}.}
\item{fs}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1000}.}
\item{z}{Optional; may be \code{NULL}. Passed to \code{.morie_rg_polyz}.}
}
\description{
so it notches DC and every harmonic of fs/N at once -- which is what powerline interference is.  The zero at DC is not optional, so a comb removes the mean along with the interference.
}
\value{
A list with \code{b}, \code{a}, \code{H}, \code{period_samples}, \code{fs}, \code{notch_frequencies_hz}, \code{n_zeros}, \code{notch_spacing_hz}, \code{dc_gain}, \code{removes_dc_as_well}, \code{fir}, \code{linear_phase}, \code{method}.
}
\keyword{internal}
\examples{
Comb(period_samples = 5L)
}
