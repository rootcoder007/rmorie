% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Cepstrum}
\alias{Cepstrum}
\title{Real cepstrum: c(n) = IDFT(log|DFT(x)|).  Keeps only the magnitude,}
\usage{
Cepstrum(x)
}
\arguments{
\item{x}{See Usage.}
}
\description{
so it discards the phase and is NOT invertible -- the whole difference from the complex cepstrum of eq (4.64).  It still shows the echo impulses of eq (4.80), which is what it is used for.
}
\value{
A list with \code{cepstrum}, \code{log_magnitude}, \code{n}, \code{zero_bins}, \code{invertible}, \code{method}.
}
\keyword{internal}
