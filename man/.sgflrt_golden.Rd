% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sgflrt_golden}
\alias{.sgflrt_golden}
\title{.sgflrt_golden}
\usage{
.sgflrt_golden(f, lo, hi, iters = 16L)
}
\arguments{
\item{f}{See Usage.}
\item{lo}{See Usage.}
\item{hi}{See Usage.}
\item{iters}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{16L}.}
}
\description{
A step of the sgflrt_native implementation. Called by \code{morie_sgflrt_spatial_glmm_fit}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
