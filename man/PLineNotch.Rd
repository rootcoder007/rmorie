% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{PLineNotch}
\alias{PLineNotch}
\title{Eq (3.150): H(z) = 1 - 2 cos(wo) z^-1 + z^-2, divided by its DC gain}
\usage{
PLineNotch(x, fs, f0 = 60, harmonics = 1)
}
\arguments{
\item{x}{See Usage.}
\item{fs}{See Usage.}
\item{f0}{Defaults to \code{60}.}
\item{harmonics}{Defaults to \code{1}.}
}
\description{
H(1) = 2 - 2 cos(wo) so the passband gain at DC is unity.  Extra zeros at n fo / fs make it a comb for the harmonics of a non-sinusoidal mains waveform.
}
\value{
A list with \code{y}, \code{coeffs}, \code{notched}, \code{f0}, \code{fs}, \code{n}, \code{method}.
}
\keyword{internal}
