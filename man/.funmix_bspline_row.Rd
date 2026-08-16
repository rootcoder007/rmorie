% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.funmix_bspline_row}
\alias{.funmix_bspline_row}
\title{One row of the B-spline design, by the Cox-de Boor recursion}
\usage{
.funmix_bspline_row(x, kn, degree, n_basis, tmax)
}
\arguments{
\item{x}{Numeric; combined arithmetically in the body.}
\item{kn}{A vector; its length is taken and its elements indexed.}
\item{degree}{A count; the body uses it as \code{seq_len(...)}.}
\item{n_basis}{A count; the body uses it as \code{seq_len(...)}.}
\item{tmax}{See Usage.}
}
\description{
A step of the funmix_native implementation. Called by \code{morie_funmix_functional_mixture}. See the file header for the source the module follows.
}
\value{
The value of \code{[}.
}
\keyword{internal}
