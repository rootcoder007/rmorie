% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{NormDist}
\alias{NormDist}
\title{Eq (10.112): d_n = |m1 - m2| / (sigma1 + sigma2).  The denominator is}
\usage{
NormDist(m1, m2, s1, s2)
}
\arguments{
\item{m1}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{m2}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{s1}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{s2}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
the SUM of the SDs, not their quadrature sum -- this is not the Fisher criterion.  The book states the limitation: d_n = 0 whenever m1 = m2, however different the dispersions, so classes separated only by variance score zero.  The divergence has no such blind spot.
}
\value{
A list with \code{dn}, \code{mean_difference}, \code{sd_sum}, \code{blind_to_variance_when_means_match}, \code{denominator_is_the_sum_not_the_quadrature_sum}, \code{method}.
}
\keyword{internal}
\examples{
NormDist(0, 2, 1, 1)
}
