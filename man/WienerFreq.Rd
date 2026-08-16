% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{WienerFreq}
\alias{WienerFreq}
\title{Eq. (3.176): W(w) = S_xd(w) / S_xx(w).  Where the denominator}
\usage{
WienerFreq(sxx, sxd)
}
\arguments{
\item{sxx}{See Usage.}
\item{sxd}{See Usage.}
}
\description{
vanishes the ratio is undefined; returning zero there is a choice, and it is flagged so a caller can tell it apart from a genuine zero response.
}
\value{
A list with \code{W}, \code{magnitude}, \code{undetermined_bins}, \code{n_undetermined}, \code{zero_where_undetermined}, \code{n}, \code{method}.
}
\keyword{internal}
