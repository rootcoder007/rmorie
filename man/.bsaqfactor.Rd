% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.bsaqfactor}
\alias{.bsaqfactor}
\title{3 dB bandwidth and Q = f_peak / bandwidth of the peak at fpeak}
\usage{
.bsaqfactor(freqs, power, fpeak)
}
\arguments{
\item{freqs}{A vector; its length is taken and its elements indexed.}
\item{power}{A vector; indexed elementwise.}
\item{fpeak}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the rangayyan_phys implementation. Called by \code{CadSpec}, \code{SpeechFeat}, \code{ValvePcg}. See the file header for the source the module follows.
}
\value{
A list with \code{bandwidth_hz}, \code{q}.
}
\keyword{internal}
