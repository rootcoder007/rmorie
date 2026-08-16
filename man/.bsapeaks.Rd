% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.bsapeaks}
\alias{.bsapeaks}
\title{Local maxima of power, strongest first, at least minsep Hz apart}
\usage{
.bsapeaks(freqs, power, count = 3L, minsep = 0)
}
\arguments{
\item{freqs}{A vector; indexed elementwise.}
\item{power}{A vector; its length is taken and its elements indexed.}
\item{count}{Defaults to \code{3L}.}
\item{minsep}{Defaults to \code{0}.}
}
\description{
A step of the rangayyan_phys implementation. Called by \code{CadAcou}, \code{CadSpec}, \code{InfantCry} and 6 others in the module. See the file header for the source the module follows.
}
\value{
A list with \code{freqs}, \code{powers}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.bsapeaks(V, V)
}
