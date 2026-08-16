% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{SyncAvg}
\alias{SyncAvg}
\title{Eqs (3.95)-(3.96): y_k = x_k + eta_k, and the sum over k separates}
\usage{
SyncAvg(observations)
}
\arguments{
\item{observations}{See Usage.}
}
\description{
into a signal sum that grows as M and a zero-mean noise sum that grows only as sqrt(M).  Dividing by M therefore shrinks the noise SD by 1/sqrt(M): an SNR gain of sqrt(M), or 10 log10(M) dB.
}
\value{
A list with \code{average}, \code{sd}, \code{m}, \code{n}, \code{se}, \code{snr_gain}, \code{snr_gain_db}, \code{alignment_note}, \code{method}.
}
\keyword{internal}
