% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{MurmDet}
\alias{MurmDet}
\title{MurmDet: murmur presence detection in PCG via spectral analysis}
\usage{
MurmDet(pcg, fs, threshold = 0.15, hf_band = c(150, 600))
}
\arguments{
\item{pcg}{Passed to \code{.bsapsd}.}
\item{fs}{Numeric; combined arithmetically in the body.}
\item{threshold}{Numeric; combined arithmetically in the body. Defaults to \code{0.15}.}
\item{hf_band}{A vector; indexed elementwise. Defaults to \code{c(150, 600)}.}
}
\description{
A step of the rangayyan_phys implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A vector, from \code{c}.
}
\keyword{internal}
