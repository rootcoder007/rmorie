% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.gpkron}
\alias{.gpkron}
\title{Chapter 5: Kronecker products and the multi-trait model, eq (5.5)}
\usage{
.gpkron(A, B)
}
\arguments{
\item{A}{Passed to \code{.gpmat}.}
\item{B}{Passed to \code{.gpmat}.}
}
\description{
----- Part of the helpers_gp_core implementation; see the file header for the source it follows.
}
\value{
The value of \code{kronecker}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.gpkron(V, V)
}
