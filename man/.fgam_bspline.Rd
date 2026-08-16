% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.fgam_bspline}
\alias{.fgam_bspline}
\title{.fgam_bspline}
\usage{
.fgam_bspline(x, kn, n_basis, degree = 3L)
}
\arguments{
\item{x}{Numeric; combined arithmetically in the body.}
\item{kn}{A vector; its length is taken and its elements indexed.}
\item{n_basis}{A count; the body uses it as \code{seq_len(...)}.}
\item{degree}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{3L}.}
}
\description{
A step of the fgam_native implementation. Called by \code{morie_fgam_functional_gam}. See the file header for the source the module follows.
}
\value{
The value of \code{[}.
}
\keyword{internal}
