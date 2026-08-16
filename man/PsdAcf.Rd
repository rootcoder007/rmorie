% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{PsdAcf}
\alias{PsdAcf}
\title{Eq. (4.30): the PSD is the DFT of the ACF.  It holds exactly for the}
\usage{
PsdAcf(x)
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
CIRCULAR autocorrelation.  The linear (biased) ACF gives a smoothed version instead, and the size of that difference is returned -- the distinction is the reason the periodogram is inconsistent.
}
\value{
A list with \code{psd}, \code{via_circular_acf}, \code{acf_circular}, \code{acf_linear}, \code{max_difference}, \code{holds}, \code{linear_difference}, \code{linear_acf_is_smoothed}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
PsdAcf(V)
}
