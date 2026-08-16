% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.s4_clip}
\alias{.s4_clip}
\title{.s4_clip}
\usage{
.s4_clip(v, lo, hi)
}
\arguments{
\item{v}{See Usage.}
\item{lo}{See Usage.}
\item{hi}{See Usage.}
}
\description{
A step of the s04core implementation. Called by \code{.s4_glmbin}, \code{.s4_tmle}, \code{.tmlmpi_cdf_bank} and 25 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{pmin}.
}
\keyword{internal}
\examples{
.s4_clip(v = c(1, 2, 3, 4, 5, 6, 7, 8), lo = c(1, 2, 3, 4, 5, 6, 7, 8), hi = c(1, 2, 3, 4, 5, 6, 7, 8))
}
