% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.b1_bound}
\alias{.b1_bound}
\title{.b1_bound}
\usage{
.b1_bound(v, lo, hi)
}
\arguments{
\item{v}{See Usage.}
\item{lo}{See Usage.}
\item{hi}{See Usage.}
}
\description{
A step of the b1_tmle implementation. Called by \code{.b1_logit}, \code{.b1_target}, \code{Comptml} and 3 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{pmin}.
}
\keyword{internal}
\examples{
.b1_bound(v = c(1, 2, 3, 4, 5, 6, 7, 8), lo = c(1, 2, 3, 4, 5, 6, 7, 8), hi = c(1, 2, 3, 4, 5, 6, 7, 8))
}
