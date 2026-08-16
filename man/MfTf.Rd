% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{MfTf}
\alias{MfTf}
\title{Eq (4.48): H(f) = K X*(f) exp(-j 2 pi f t0).  The CONJUGATE is what}
\usage{
MfTf(X, freqs, t0, gain = 1)
}
\arguments{
\item{X}{See Usage.}
\item{freqs}{See Usage.}
\item{t0}{See Usage.}
\item{gain}{Defaults to \code{1}.}
}
\description{
cancels the signal\\'s phase so every component arrives in step at t0 -- that coherent addition is the whole gain of the method.
}
\value{
A list with \code{H}, \code{freqs}, \code{t0}, \code{gain}, \code{magnitude}, \code{conjugate_of_signal}, \code{method}.
}
\keyword{internal}
\examples{
MfTf(X = c(1, 2, 3, 4, 5, 6, 7, 8), freqs = c(1, 2, 3, 4, 5, 6, 7, 8), t0 = c(1, 2, 3, 4, 5, 6, 7, 8))
}
