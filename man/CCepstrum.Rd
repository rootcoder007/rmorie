% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{CCepstrum}
\alias{CCepstrum}
\title{Eqs (4.63)-(4.64): the inverse transform of log|X| + j angle(X)}
\usage{
CCepstrum(x)
}
\arguments{
\item{x}{See Usage.}
}
\description{
The phase is unwrapped over k = 0..N/2 and mirrored by odd symmetry; the linear term (the z^r delay of eq 4.68) is r = round(phase(pi)/pi).
}
\value{
A list with \code{cepstrum}, \code{log_magnitude}, \code{phase}, \code{detrended_phase}, \code{linear_phase_removed}, \code{delay_removed}, \code{n}, \code{method}.
}
\keyword{internal}
