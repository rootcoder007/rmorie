% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{CDiff3Mag}
\alias{CDiff3Mag}
\title{Eq (3.130): (1/T)|sin w|.  Nought at BOTH ends -- at DC from the}
\usage{
CDiff3Mag(omega, T = 1)
}
\arguments{
\item{omega}{See Usage.}
\item{T}{Defaults to \code{1}.}
}
\description{
highpass factor, at Nyquist from the moving-average factor -- peaking at w = pi/2.
}
\value{
A list with \code{magnitude}, \code{omega}, \code{T}, \code{dc_gain}, \code{nyquist_gain}, \code{peak_at}, \code{bandpass}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
CDiff3Mag(V)
}
