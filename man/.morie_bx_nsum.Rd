% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_bx_nsum}
\alias{.morie_bx_nsum}
\title{Plain sequential double accumulation, NOT compensated: mirrors the}
\usage{
.morie_bx_nsum(v)
}
\arguments{
\item{v}{See Usage.}
}
\description{
one place the Python arm uses `x += ...` in a loop instead of fsum, so the two arms round identically there too
}
\value{
The value of \code{s}, as built in the body.
}
\keyword{internal}
