% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_bx_gabor}
\alias{.morie_bx_gabor}
\title{Real Gabor dictionary, eqs (9.2)-(9.3), on a fixed dyadic grid so the}
\usage{
.morie_bx_gabor(n, natoms, seed = 1)
}
\arguments{
\item{n}{A count; the body uses it as \code{seq_len(...)}.}
\item{natoms}{Coerced to integer by the body, with \code{as.integer}.}
\item{seed}{Defaults to \code{1}.}
}
\description{
dictionary is reproducible without an RNG
}
\value{
A list with \code{atoms}, \code{params}.
}
\keyword{internal}
