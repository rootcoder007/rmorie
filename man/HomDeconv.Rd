% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{HomDeconv}
\alias{HomDeconv}
\title{Section 4.7.2: DFT -> complex log -> IDFT -> lifter -> DFT -> exp ->}
\usage{
HomDeconv(y, cutoff, keep = "low")
}
\arguments{
\item{y}{See Usage.}
\item{cutoff}{See Usage.}
\item{keep}{Defaults to \code{"low"}.}
}
\description{
IDFT.  Low quefrency estimates the slowly varying component (the vocal tract, the basic wavelet); high quefrency the excitation.
}
\value{
A list with \code{y}, \code{cepstrum}, \code{liftered}, \code{cutoff}, \code{keep}, \code{n}, \code{linear_phase_removed}, \code{imaginary_energy}, \code{stages}, \code{method}.
}
\keyword{internal}
