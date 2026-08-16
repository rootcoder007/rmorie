% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.t1_lcg}
\alias{.t1_lcg}
\title{Lehmer minstd -- identical stream to the Python arm}
\usage{
.t1_lcg(seed = 1)
}
\arguments{
\item{seed}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.}
}
\description{
A step of the tail1_core implementation. Called by \code{.btdir_rows}, \code{.btmbb_reps}, \code{.kvmse_rotation} and 32 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{e}, as built in the body.
}
\keyword{internal}
