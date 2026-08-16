% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{MfNoiseIn}
\alias{MfNoiseIn}
\title{Eq (4.35): S_eta_i = P_eta_i / 2.  The factor of two is the TWO-SIDED}
\usage{
MfNoiseIn(power, freqs = NULL)
}
\arguments{
\item{power}{See Usage.}
\item{freqs}{Defaults to \code{NULL}.}
}
\description{
convention: integrating the flat density over all f returns P, not twice it.  Getting it wrong is a factor of two in every SNR below.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
MfNoiseIn(power = 5L)
}
