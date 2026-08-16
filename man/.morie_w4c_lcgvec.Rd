% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_w4c_lcgvec}
\alias{.morie_w4c_lcgvec}
\title{.morie_w4c_lcgvec}
\usage{
.morie_w4c_lcgvec(n, seed)
}
\arguments{
\item{n}{A count; the body uses it as \code{seq_len(...)}.}
\item{seed}{See Usage.}
}
\description{
A step of the geron_w4c_native implementation. Called by \code{.morie_w4c_lcg_uniform}, \code{.morie_w4c_nmf}, \code{morie_geron_pvt} and 1 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
