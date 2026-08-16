% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{MfNoiseOut}
\alias{MfNoiseOut}
\title{Eqs (4.36)-(4.37): S_eta_o = (P/2)|H|^2 and its integral.  eq (4.36)}
\usage{
MfNoiseOut(power, H, freqs = NULL, df = 1)
}
\arguments{
\item{power}{See Usage.}
\item{H}{See Usage.}
\item{freqs}{Defaults to \code{NULL}.}
\item{df}{Defaults to \code{1}.}
}
\description{
holds only because the input was white, so the density factored out.
}
\value{
A list with \code{psd}, \code{power}, \code{rms}, \code{input_power}, \code{input_density}, \code{method}.
}
\keyword{internal}
