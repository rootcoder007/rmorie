% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{WienerFreqR}
\alias{WienerFreqR}
\title{Eq. (3.175): W(w) S_xx(w) = S_xd(w).  Bins where S_xx vanishes carry}
\usage{
WienerFreqR(W, sxx, sxd) 
}
\arguments{
\item{W}{Coerced to complex by the body, with \code{as.complex}.}
\item{sxx}{Coerced to complex by the body, with \code{as.complex}.}
\item{sxd}{Coerced to complex by the body, with \code{as.complex}.}
}
\description{
no information about W and are listed, not silently satisfied.
}
\value{
A list with \code{lhs}, \code{sxd}, \code{max_difference}, \code{holds}, \code{undetermined_bins}, \code{n_undetermined}, \code{method}.
}
\keyword{internal}
\examples{
WienerFreqR(W = c(1, 2, 3, 4, 5, 6, 7, 8), sxx = c(1, 2, 3, 4, 5, 6, 7, 8),
  sxd = c(1, 2, 3, 4, 5, 6, 7, 8))
}
