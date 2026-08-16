% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.fgam_knots}
\alias{.fgam_knots}
\title{.fgam_knots}
\usage{
.fgam_knots(lo, hi, n_basis, degree = 3L)
}
\arguments{
\item{lo}{A count; the body uses it as \code{rep(...)}.}
\item{hi}{A count; the body uses it as \code{rep(...)}.}
\item{n_basis}{Numeric; combined arithmetically in the body.}
\item{degree}{Numeric; combined arithmetically in the body. Defaults to \code{3L}.}
}
\description{
A step of the fgam_native implementation. Called by \code{morie_fgam_functional_gam}. See the file header for the source the module follows.
}
\value{
A vector, from \code{c}.
}
\keyword{internal}
