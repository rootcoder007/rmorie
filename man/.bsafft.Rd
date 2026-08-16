% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.bsafft}
\alias{.bsafft}
\title{In-place iterative radix-2 Cooley-Tukey FFT; len must be a power of 2}
\usage{
.bsafft(re, im)
}
\arguments{
\item{re}{A vector; its length is taken and its elements indexed.}
\item{im}{A vector; indexed elementwise.}
}
\description{
A step of the rangayyan_phys implementation. Called by \code{.bsapsd}, \code{PcgEeg}. See the file header for the source the module follows.
}
\value{
A list with \code{re}, \code{im}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.bsafft(V, V)
}
