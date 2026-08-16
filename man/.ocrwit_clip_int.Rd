% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ocrwit_clip_int}
\alias{.ocrwit_clip_int}
\title{.ocrwit_clip_int}
\usage{
.ocrwit_clip_int(v, lo, hi)
}
\arguments{
\item{v}{Passed to \code{round}.}
\item{lo}{Coerced to integer by the body, with \code{as.integer}.}
\item{hi}{Coerced to integer by the body, with \code{as.integer}.}
}
\description{
A step of the ocrwit_native implementation. Called by \code{normalise_bbox}. See the file header for the source the module follows.
}
\value{
The value of \code{v}, as built in the body.
}
\keyword{internal}
