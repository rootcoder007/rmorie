% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.funmix_knots}
\alias{.funmix_knots}
\title{Clamped uniform knot vector for n_basis B-splines}
\usage{
.funmix_knots(tmin, tmax, n_basis, degree)
}
\arguments{
\item{tmin}{A count; the body uses it as \code{rep(...)}.}
\item{tmax}{A count; the body uses it as \code{rep(...)}.}
\item{n_basis}{Numeric; combined arithmetically in the body.}
\item{degree}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the funmix_native implementation. Called by \code{morie_funmix_functional_mixture}. See the file header for the source the module follows.
}
\value{
A vector, from \code{c}.
}
\keyword{internal}
