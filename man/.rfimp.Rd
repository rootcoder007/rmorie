% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.rfimp}
\alias{.rfimp}
\title{Within-node sum of squares, the least-square criterion of p. 643}
\usage{
.rfimp(Y, rows, q)
}
\arguments{
\item{Y}{A matrix; indexed by row and column.}
\item{rows}{A vector; its length is taken.}
\item{q}{A count; the body uses it as \code{seq_len(...)}.}
}
\description{
A step of the mvsml_rf_shared implementation. Called by \code{.rfgrow}. See the file header for the source the module follows.
}
\value{
The value of \code{tot}, as built in the body.
}
\keyword{internal}
\examples{
.rfimp(Y = c(1, 2, 3, 4, 5, 6, 7, 8), rows = c(1, 2, 3, 4, 5, 6, 7, 8), q = 0.5)
}
