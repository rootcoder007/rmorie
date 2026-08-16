% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Csd}
\alias{Csd}
\title{Eqs (4.30)-(4.31): S_xx = |X|^2 and S_xy = X Y*.  Both routes to the}
\usage{
Csd(x, y, fs = 1)
}
\arguments{
\item{x}{See Usage.}
\item{y}{See Usage.}
\item{fs}{Defaults to \code{1}.}
}
\description{
CSD -- the transform of the CCF and the product -- are computed and compared; they agree only for the full CIRCULAR lag range, since a truncated CCF gives a smoothed CSD, a different estimator.
}
\value{
A list with \code{csd}, \code{via_ccf}, \code{ccf_circular}, \code{freqs}, \code{max_difference}, \code{agrees}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Csd(V, V)
}
