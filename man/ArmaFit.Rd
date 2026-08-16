% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{ArmaFit}
\alias{ArmaFit}
\title{Section 7.7: H(z) = B(z)/A(z), needed when the signal has spectral}
\usage{
ArmaFit(x, p, q, fs = 1)
}
\arguments{
\item{x}{See Usage.}
\item{p}{See Usage.}
\item{q}{See Usage.}
\item{fs}{Defaults to \code{1}.}
}
\description{
nulls as well as resonances -- an all-pole model can only make peaks. Fitted in two stages (AR from the ACF, MA from the residual ACF), which is NOT joint maximum likelihood and is biased when the zeros sit close to the poles.  The stage structure is stated, not presented as an optimal fit.
}
\value{
A list with \code{a}, \code{b}, \code{p}, \code{q}, \code{gain}, \code{poles}, \code{zeros}, \code{stable}, \code{ar_error}, \code{two_stage}, \code{method}.
}
\keyword{internal}
