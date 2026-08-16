% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.solve}
\alias{.solve}
\title{.solve}
\usage{
.solve(A, b)
}
\arguments{
\item{A}{A matrix; passed to \code{nrow}.}
\item{b}{Passed to \code{cbind}.}
}
\description{
A step of the birl_native implementation. Called by \code{policy_values}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
