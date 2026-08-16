% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_cf_tau}
\alias{.morie_cf_tau}
\title{.morie_cf_tau}
\usage{
.morie_cf_tau(y, d)
}
\arguments{
\item{y}{A vector; indexed elementwise.}
\item{d}{Passed to \code{==}.}
}
\description{
A step of the causal_forest_honest implementation. Called by \code{.morie_cf_grow}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_cf_tau(V, V)
}
