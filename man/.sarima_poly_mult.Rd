% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sarima_poly_mult}
\alias{.sarima_poly_mult}
\title{.sarima_poly_mult}
\usage{
.sarima_poly_mult(a, b)
}
\arguments{
\item{a}{A vector; its length is taken and its elements indexed.}
\item{b}{A vector; its length is taken and its elements indexed.}
}
\description{
A step of the sarima_native implementation. Called by \code{.sarima_diff_poly}, \code{expand_polynomials}, \code{forecast}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
