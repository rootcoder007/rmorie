% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{VagTfd}
\alias{VagTfd}
\title{Section 9.6 applied to VAG in Section 9.9.  Bilinear TFDs buy}
\usage{
VagTfd(x, fs, natoms = 12, nfreq = 32, ntime = NULL, lag = 12)
}
\arguments{
\item{x}{A vector; its length is taken.}
\item{fs}{Numeric; combined arithmetically in the body.}
\item{natoms}{Defaults to \code{12}.}
\item{nfreq}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{32}.}
\item{ntime}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
\item{lag}{Numeric; combined arithmetically in the body. Defaults to \code{12}.}
}
\description{
resolution with cross-terms; decompose first and the interaction is known, so the cross-term double sum of eq (9.15) is simply left out -- that omission is what makes the TFD adaptive rather than smoothed. Features EP (9.79), ESP (9.80), FP (9.81), FSP (9.82).
}
\value{
A list with \code{tfd}, \code{times}, \code{frequencies}, \code{ep}, \code{esp}, \code{fp}, \code{fsp}, \code{coefficients}, \code{method}.
}
\keyword{internal}
\examples{
VagTfd(x = c(1, 2, 3, 4, 5, 6, 7, 8), fs = 5L)
}
