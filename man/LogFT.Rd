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
\item{x}{See Usage.}
\item{p}{See Usage.}
\item{omega}{See Usage.}
\item{t}{Defaults to \code{NULL}.}
\item{dt}{Defaults to \code{NULL}.}
}
\description{
Y_l(omega) = X_l(omega) + P_l(omega).  Eq (4.59) needs both factors nonzero, so zeros are rejected rather than yielding -Inf.
}
\value{
A list with \code{y}, \code{Yl}, \code{Xl}, \code{Pl}, \code{max_difference}, \code{additive}, \code{method}.
}
\keyword{internal}
