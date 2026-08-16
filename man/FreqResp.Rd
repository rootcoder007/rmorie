% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{FreqResp}
\alias{FreqResp}
\title{Evaluated on a uniform grid from DC to NYQUIST inclusive -- the}
\usage{
FreqResp(b, a = NULL, fs = 1000, n_freqs = 512)
}
\arguments{
\item{b}{See Usage.}
\item{a}{Defaults to \code{NULL}.}
\item{fs}{Defaults to \code{1000}.}
\item{n_freqs}{Defaults to \code{512}.}
}
\description{
one-sided response, since for real coefficients the other half is the conjugate mirror.  `a` follows the eq (3.67) convention with a_0 = 1 included, the form BwLp and BwHp return.
}
\value{
A list with \code{f}, \code{H}, \code{magnitude}, \code{magnitude_db}, \code{phase}, \code{fs}, \code{n_freqs}, \code{one_sided}, \code{includes_nyquist}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
FreqResp(V)
}
