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
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{p}{Coerced to integer by the body, with \code{as.integer}.}
\item{q}{Coerced to integer by the body, with \code{as.integer}.}
\item{fs}{Accepted by the signature and not used anywhere in the body. Defaults to \code{1}.}
}
\description{
nulls as well as resonances -- an all-pole model can only make peaks. Fitted in two stages (AR from the ACF, MA from the residual ACF), which is NOT joint maximum likelihood and is biased when the zeros sit close to the poles.  The stage structure is stated, not presented as an optimal fit.
}
\value{
A list with \code{a}, \code{b}, \code{p}, \code{q}, \code{gain}, \code{poles}, \code{zeros}, \code{stable}, \code{ar_error}, \code{two_stage}, \code{method}.
}
\keyword{internal}
\examples{
ArmaFit(x = c(1, 2, 3, 4, 5, 6, 7, 8), p = 1, q = 0.5)
}
