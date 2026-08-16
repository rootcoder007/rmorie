% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.abcgp.corr}
\alias{.abcgp.corr}
\title{.abcgp.corr}
\usage{
.abcgp.corr(a, b, lengthscale, kernel)
}
\arguments{
\item{a}{Numeric; combined arithmetically in the body.}
\item{b}{Numeric; combined arithmetically in the body.}
\item{lengthscale}{Numeric; combined arithmetically in the body.}
\item{kernel}{One of \code{"matern32"}, \code{"sqexp"}.}
}
\description{
A step of the abcgp_native implementation. Called by \code{.abcgp.gp_fit}, \code{.abcgp.gp_predict}, \code{.abcgp.profile_nll}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
