% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.gpannsse}
\alias{.gpannsse}
\title{Chapter 10: ANN loss, eq (10.5)}
\usage{
.gpannsse(y_hat, y)
}
\arguments{
\item{y_hat}{Passed to \code{.gpmat}.}
\item{y}{Passed to \code{.gpmat}.}
}
\description{
-------------------------------------- Part of the helpers_gp_core implementation; see the file header for the source it follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.gpannsse(V, V)
}
