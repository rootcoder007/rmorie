% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.groebn_scale}
\alias{.groebn_scale}
\title{.groebn_scale}
\usage{
.groebn_scale(f, c)
}
\arguments{
\item{f}{Iterated over elementwise, with \code{lapply}.}
\item{c}{Passed to \code{.groebn_as_fr}.}
}
\description{
A step of the groebn_native implementation. Called by \code{.groebn_reduce_basis}. See the file header for the source the module follows.
}
\value{
The value of \code{lapply}.
}
\keyword{internal}
