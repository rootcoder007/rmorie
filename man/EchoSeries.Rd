% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{EchoSeries}
\alias{EchoSeries}
\title{Eqs (4.79)-(4.80): the complex cepstrum of a wavelet plus one echo}
\usage{
EchoSeries(a, n0, terms = 10, omega = NULL)
}
\arguments{
\item{a}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{n0}{Coerced to integer by the body, with \code{as.integer}.}
\item{terms}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{10}.}
\item{omega}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
is the wavelet\'s cepstrum plus impulses at n0 and its multiples, amplitudes (-1)^(k+1) a^k / k.  The expansion needs |a| < 1, which the book states and which is enforced.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
EchoSeries(0.5, 8, terms = 4)
}
