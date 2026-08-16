% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_bvn_cdf}
\alias{.morie_bvn_cdf}
\title{Bivariate normal CDF by one-dimensional quadrature over the}
\usage{
.morie_bvn_cdf(x, y, rho)
}
\arguments{
\item{x}{Numeric; passed to \code{max}.}
\item{y}{Numeric; passed to \code{max}.}
\item{rho}{Numeric; combined arithmetically in the body.}
}
\description{
conditional normal -- avoids a mvtnorm dependency.
}
\value{
The value of \code{$}.
}
\keyword{internal}
