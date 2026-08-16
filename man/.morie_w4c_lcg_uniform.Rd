% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_w4c_lcg_uniform}
\alias{.morie_w4c_lcg_uniform}
\title{.morie_w4c_lcg_uniform}
\usage{
.morie_w4c_lcg_uniform(rows, cols, seed, scale)
}
\arguments{
\item{rows}{A count; the body uses it as \code{matrix(...)}.}
\item{cols}{A count; the body uses it as \code{matrix(...)}.}
\item{seed}{Passed to \code{.morie_w4c_lcgvec}.}
\item{scale}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the geron_w4c_native implementation. Called by \code{morie_geron_regression_mlp}. See the file header for the source the module follows.
}
\value{
A matrix, from \code{matrix}.
}
\keyword{internal}
\examples{
.morie_w4c_lcg_uniform(rows = 5L, cols = 5L, seed = 5L, scale = TRUE)
}
