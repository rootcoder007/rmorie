% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Ma8RecTf}
\alias{Ma8RecTf}
\title{Eq (3.121): a pole at z = 1 cancelled by one of the numerator\'s}
\usage{
Ma8RecTf(z)
}
\arguments{
\item{z}{See Usage.}
}
\description{
zeros, so the filter is still FIR despite the recursive implementation.  At z = 1 the ratio is 0/0 and the limit is the DC gain, 1.
}
\value{
A list with \code{H}, \code{z}, \code{pole_at_dc_cancelled_by_a_zero}, \code{still_fir}, \code{dc_gain}, \code{removable_singularity_at_z_equals_one}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Ma8RecTf(V)
}
