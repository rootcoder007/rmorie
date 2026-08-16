% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_bvt_cdf}
\alias{.morie_bvt_cdf}
\title{Bivariate t CDF as a chi-square scale mixture of bivariate normals}
\usage{
.morie_bvt_cdf(x, y, rho, nu)
}
\arguments{
\item{x}{Numeric; combined arithmetically in the body.}
\item{y}{Numeric; combined arithmetically in the body.}
\item{rho}{Passed to \code{.morie_bvn_cdf}.}
\item{nu}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the copula_native implementation. Called by \code{morie_copula_cdf}. See the file header for the source the module follows.
}
\value{
The value of \code{$}.
}
\keyword{internal}
