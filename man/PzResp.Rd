% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{PzResp}
\alias{PzResp}
\title{Eqs (3.71)-(3.73): on the unit circle the magnitude is the product}
\usage{
PzResp(zeros, poles, omega, gain = 1)
}
\arguments{
\item{zeros}{See Usage.}
\item{poles}{See Usage.}
\item{omega}{See Usage.}
\item{gain}{Numeric; passed to \code{abs}. Defaults to \code{1}.}
}
\description{
of distances to the zeros over the product of distances to the poles, and the phase is (M-N) angle(z0) plus the zero angles minus the pole angles.  A zero ON the circle sends one distance to zero -- a spectral null; a pole near it makes one distance small -- a resonance.  The distances are returned so that reading can be made.
}
\value{
A list with \code{H}, \code{magnitude}, \code{phase}, \code{zero_distances}, \code{pole_distances}, \code{omega}, \code{magnitude_matches_product}, \code{method}.
}
\keyword{internal}
\examples{
PzResp(zeros = c(1, 2, 3, 4, 5, 6, 7, 8), poles = c(1, 2, 3, 4, 5, 6, 7, 8), omega = c(1, 2, 3, 4, 5, 6, 7, 8))
}
