% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{MfRatio}
\alias{MfRatio}
\title{Eq (4.41).  Dividing by the constant E_x is what turns eq (4.39) into}
\usage{
MfRatio(X, H, freqs, t0, noise_power)
}
\arguments{
\item{X}{See Usage.}
\item{H}{See Usage.}
\item{freqs}{See Usage.}
\item{t0}{See Usage.}
\item{noise_power}{See Usage.}
}
\description{
something Schwarz\\'s inequality applies to; by eq (4.46) the ratio cannot exceed 2/P_eta_i, with equality exactly at eq (4.48).
}
\value{
A list with \code{ratio}, \code{bound}, \code{optimality}, \code{numerator}, \code{energy_h}, \code{energy_x}, \code{method}.
}
\keyword{internal}
