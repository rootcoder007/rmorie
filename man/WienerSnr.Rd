% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{WienerSnr}
\alias{WienerSnr}
\title{Eq. (3.186): W = S_d / (S_d + S_eta).  Three properties the book}
\usage{
WienerSnr(sd, seta)
}
\arguments{
\item{sd}{See Usage.}
\item{seta}{See Usage.}
}
\description{
stresses and this checks: zero where the signal is absent (nothing to restore), unity where the noise is absent (nothing to suppress), and falling monotonically with the SNR in between.
}
\value{
A list with \code{W}, \code{snr}, \code{undetermined_bins}, \code{zero_where_signal_absent}, \code{unity_where_noise_absent}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
WienerSnr(V, V)
}
