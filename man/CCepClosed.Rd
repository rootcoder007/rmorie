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
\item{gain}{Coerced to complex by the body, with \code{as.complex}.}
\item{zeros_in}{Coerced to complex by the body, with \code{as.complex}.}
\item{zeros_out}{Coerced to complex by the body, with \code{as.complex}.}
\item{poles_in}{Coerced to complex by the body, with \code{as.complex}.}
\item{poles_out}{Coerced to complex by the body, with \code{as.complex}.}
\item{nmax}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{32}.}
}
\description{
A step of the rangayyan_cep implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{cepstrum}, \code{quefrency}, \code{c0}, \code{positive}, \code{negative}, \code{causal}, \code{anticausal}, \code{infinite_duration}, \code{nmax}, \code{method}.
}
\keyword{internal}
\examples{
CCepClosed(gain = 5L, zeros_in = c(1, 2, 3, 4, 5, 6, 7, 8), zeros_out = c(1, 2, 3, 4, 5, 6, 7, 8), poles_in = c(1, 2, 3, 4, 5, 6, 7, 8), poles_out = c(1, 2, 3, 4, 5, 6, 7, 8))
}
