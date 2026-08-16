% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mehtad_solve_normal}
\alias{.mehtad_solve_normal}
\title{.mehtad_solve_normal}
\usage{
.mehtad_solve_normal(A, d, rhs, ridge = 1e-11)
}
\arguments{
\item{A}{A matrix; passed to \code{as.matrix}.}
\item{d}{A count; the body uses it as \code{rep(...)}.}
\item{rhs}{See Usage.}
\item{ridge}{Numeric; combined arithmetically in the body. Defaults to \code{1e-11}.}
}
\description{
A step of the mehtad_native implementation. Called by \code{newton_direction}. See the file header for the source the module follows.
}
\value{
The value of \code{backsolve}.
}
\keyword{internal}
