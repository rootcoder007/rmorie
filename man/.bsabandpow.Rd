% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.bsabandpow}
\alias{.bsabandpow}
\title{Total power in the half-open band [lo, hi) Hz}
\usage{
.bsabandpow(freqs, power, lo, hi)
}
\arguments{
\item{freqs}{See Usage.}
\item{power}{A vector; indexed elementwise.}
\item{lo}{See Usage.}
\item{hi}{See Usage.}
}
\description{
A step of the rangayyan_phys implementation. Called by \code{ApneaDet}, \code{CadAcou}, \code{CadSpec} and 7 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{.morie_fsum}.
}
\keyword{internal}
