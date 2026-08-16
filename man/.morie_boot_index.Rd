% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_boot_index}
\alias{.morie_boot_index}
\title{Boot:::ordinary.array -- single- or multi-stratum R x n index matrix}
\usage{
.morie_boot_index(n, R, strata = NULL)
}
\arguments{
\item{n}{A count; the body uses it as \code{seq_len(...)}.}
\item{R}{A count; the body uses it as \code{rep(...)}.}
\item{strata}{Optional; may be \code{NULL}. Passed to \code{is.null}.}
}
\description{
A step of the boot_native implementation. Called by \code{morie_boot}. See the file header for the source the module follows.
}
\value{
The value of \code{output}, as built in the body.
}
\keyword{internal}
\examples{
.morie_boot_index(n = 5L, R = 5L)
}
