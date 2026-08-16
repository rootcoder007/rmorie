% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{CCepClosed}
\alias{CCepClosed}
\title{CCepClosed}
\usage{
CCepClosed(gain, zeros_in, zeros_out, poles_in, poles_out, nmax = 32)
}
\arguments{
\item{gain}{See Usage.}
\item{zeros_in}{See Usage.}
\item{zeros_out}{See Usage.}
\item{poles_in}{See Usage.}
\item{poles_out}{See Usage.}
\item{nmax}{Defaults to \code{32}.}
}
\description{
Part of the rangayyan_cep implementation; see the file header for the source it follows.
}
\value{
A list with \code{cepstrum}, \code{quefrency}, \code{c0}, \code{positive}, \code{negative}, \code{causal}, \code{anticausal}, \code{infinite_duration}, \code{nmax}, \code{method}.
}
\keyword{internal}
\examples{
CCepClosed(gain = 5L, zeros_in = c(1, 2, 3, 4, 5, 6, 7, 8), zeros_out = c(1, 2, 3, 4, 5, 6, 7, 8), poles_in = c(1, 2, 3, 4, 5, 6, 7, 8), poles_out = c(1, 2, 3, 4, 5, 6, 7, 8))
}
