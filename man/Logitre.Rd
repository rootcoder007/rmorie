% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Logitre}
\alias{Logitre}
\title{Binary spatial regression with random effect (Lawson eq. 17.1)}
\usage{
Logitre(gamma0, gamma1, d, gamma2, x, R) 
}
\description{
Binary spatial regression with random effect (Lawson eq. 17.1)
}
\arguments{
\item{gamma0}{Argument \code{gamma0}; see Usage.}
\item{gamma1}{Argument \code{gamma1}; see Usage.}
\item{d}{Argument \code{d}; see Usage.}
\item{gamma2}{Argument \code{gamma2}; see Usage.}
\item{x}{Argument \code{x}; see Usage.}
\item{R}{Argument \code{R}; see Usage.}
}
\value{
A list with \code{eta}, \code{p}, \code{n}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_unclr_dft_amp(V)
Logitre(gamma0 = V, gamma1 = V, d = V, gamma2 = V, x = V, R = V)
}
