% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{ARtoCepstrum}
\alias{ARtoCepstrum}
\title{Rangayyan eq. (7.65):}
\usage{
ARtoCepstrum(a_coeffs, gain = NULL) 
}
\arguments{
\item{a_coeffs}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{gain}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
h(1) = -a1; h(n) = -a_n - sum_\{k=1\}^\{n-1\} (1 - k/n) a_k h(n-k) Going through the AR coefficients avoids the phase unwrapping the FFT cepstrum needs (Section 4.7.3).
}
\value{
A list with \code{cepstrum}, \code{c0}, \code{order}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
ARtoCepstrum(V)
}
