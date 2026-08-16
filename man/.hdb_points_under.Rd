% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.hdb_points_under}
\alias{.hdb_points_under}
\title{.hdb_points_under}
\usage{
.hdb_points_under(node, children, n)
}
\arguments{
\item{node}{Passed to \code{c}.}
\item{children}{A vector; indexed elementwise.}
\item{n}{Passed to \code{<}.}
}
\description{
A step of the hdbsc_native implementation. Called by \code{morie_hdbsc}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
.hdb_points_under(node = list(a = 1, b = 2), children = c(1, 2, 3, 4, 5, 6, 7, 8), n = 5L)
}
