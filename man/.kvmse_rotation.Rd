% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.kvmse_rotation}
\alias{.kvmse_rotation}
\title{.kvmse_rotation}
\usage{
.kvmse_rotation(d, seed)
}
\arguments{
\item{d}{A count; the body uses it as \code{seq_len(...)}.}
\item{seed}{Passed to \code{.t1_lcg}.}
}
\description{
A step of the tqkmse implementation. Called by \code{Kvmse}, \code{Kvquant}, \code{Vcquant}. See the file header for the source the module follows.
}
\value{
The value of \code{Q}, as built in the body.
}
\keyword{internal}
