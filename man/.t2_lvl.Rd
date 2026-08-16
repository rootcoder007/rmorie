% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.t2_lvl}
\alias{.t2_lvl}
\title{.t2_lvl}
\usage{
.t2_lvl(a, b)
}
\arguments{
\item{a}{Passed to \code{c}.}
\item{b}{Passed to \code{c}.}
}
\description{
A step of the tail2_t01 implementation. Called by \code{KappaCoh}, \code{KappaWt}. See the file header for the source the module follows.
}
\value{
A vector, from \code{sort}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.t2_lvl(V, V)
}
