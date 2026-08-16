% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.bnsadt_pub_cdf}
\alias{.bnsadt_pub_cdf}
\title{P(X <= x | Theta = theta) for a PUBLISHED study. Numerator and}
\usage{
.bnsadt_pub_cdf(x, theta, sigma, family, params)
}
\arguments{
\item{x}{See Usage.}
\item{theta}{Numeric; combined arithmetically in the body.}
\item{sigma}{Numeric; combined arithmetically in the body.}
\item{family}{Passed to \code{.bnsadt_betas}.}
\item{params}{Passed to \code{.bnsadt_betas}.}
}
\description{
denominator are both finite sums of normal increments, so this is exact rather than quadrature.
}
\value{
A numeric value.
}
\keyword{internal}
