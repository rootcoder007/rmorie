% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{LogFT}
\alias{LogFT}
\title{Eqs (4.58)-(4.60): y = x p, log y = log x + log p, and so}
\usage{
LogFT(x, p, omega, t = NULL, dt = NULL)
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{p}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{omega}{See Usage.}
\item{t}{The body requires: eq. (4.59) needs x(t) != 0 and p(t) != 0 for all t.}
\item{dt}{Defaults to \code{NULL}.}
}
\description{
Y_l(omega) = X_l(omega) + P_l(omega).  Eq (4.59) needs both factors nonzero, so zeros are rejected rather than yielding -Inf.
}
\value{
A list with \code{y}, \code{Yl}, \code{Xl}, \code{Pl}, \code{max_difference}, \code{additive}, \code{method}.
}
\keyword{internal}
\examples{
LogFT(x = c(1, 2, 3, 4, 5, 6, 7, 8), p = c(1, 2, 3, 4, 5, 6, 7, 8), omega = c(1, 2, 3, 4, 5, 6, 7, 8))
}
