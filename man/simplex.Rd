% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{simplex}
\alias{simplex}
\title{Two-phase primal simplex on Ax = b, x >= 0. Mirrors clpopt.simplex}
\usage{
simplex(c, A, b, rule = "bland", max_iter = 10000, initial_basis = NULL)
}
\arguments{
\item{c}{See Usage.}
\item{A}{See Usage.}
\item{b}{See Usage.}
\item{rule}{Defaults to \code{"bland"}.}
\item{max_iter}{Defaults to \code{10000}.}
\item{initial_basis}{Defaults to \code{NULL}.}
}
\description{
Part of the clpopt_native implementation; see the file header for the source it follows.
}
\value{
The value of \code{.clpopt_report}.
}
\keyword{internal}
