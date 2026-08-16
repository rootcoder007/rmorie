% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_prtcl_kalman_filter_1d}
\alias{morie_prtcl_kalman_filter_1d}
\title{morie_prtcl_kalman_filter_1d}
\usage{
morie_prtcl_kalman_filter_1d(y, a, q, c, r, m0 = 0, p0 = 1)
}
\arguments{
\item{y}{See Usage.}
\item{a}{See Usage.}
\item{q}{See Usage.}
\item{c}{See Usage.}
\item{r}{See Usage.}
\item{m0}{Defaults to \code{0}.}
\item{p0}{Defaults to \code{1}.}
}
\description{
Part of the prtcl_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{means}, \code{loglik}.
}
\keyword{internal}
\examples{
morie_prtcl_kalman_filter_1d(y = c(1, 2, 3, 4, 5, 6, 7, 8), a = c(1, 2, 3, 4, 5, 6, 7, 8), q = 0.5, c = c(1, 2, 3, 4, 5, 6, 7, 8), r = c(1, 2, 3, 4, 5, 6, 7, 8))
}
