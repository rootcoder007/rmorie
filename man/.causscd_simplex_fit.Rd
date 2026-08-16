% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.causscd_simplex_fit}
\alias{.causscd_simplex_fit}
\title{.causscd_simplex_fit}
\usage{
.causscd_simplex_fit(cols, target, penalty, iters = 2000L, tol = 1e-12)
}
\arguments{
\item{cols}{A vector; its length is taken and its elements indexed.}
\item{target}{A vector; its length is taken.}
\item{penalty}{Numeric; combined arithmetically in the body.}
\item{iters}{Defaults to \code{2000L}.}
\item{tol}{Defaults to \code{1e-12}.}
}
\description{
A step of the causscd_native implementation. Called by \code{time_weights}, \code{unit_weights}. See the file header for the source the module follows.
}
\value{
A list with \code{w}, \code{intercept}.
}
\keyword{internal}
