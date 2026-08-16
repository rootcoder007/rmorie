% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.t2_wmat}
\alias{.t2_wmat}
\title{.t2_wmat}
\usage{
.t2_wmat(weights, k)
}
\arguments{
\item{weights}{A matrix; passed to \code{as.matrix}.}
\item{k}{A count; the body uses it as \code{seq_len(...)}.}
}
\description{
A step of the tail2_t01 implementation. Called by \code{KappaWt}. See the file header for the source the module follows.
}
\value{
The value of \code{w}, as built in the body.
}
\keyword{internal}
