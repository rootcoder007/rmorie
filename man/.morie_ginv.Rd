% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_ginv}
\alias{.morie_ginv}
\title{Internal: Moore-Penrose generalized inverse (native MASS::ginv)}
\usage{
.morie_ginv(X, tol = sqrt(.Machine$double.eps))
}
\description{
Exact re-implementation of \code{MASS::ginv} for real matrices: the SVD pseudo-inverse with the same singular-value tolerance rule.
}
\value{
The value of \code{if}.
}
\keyword{internal}
