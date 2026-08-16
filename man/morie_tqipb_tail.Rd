% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_tqipb_tail}
\alias{morie_tqipb_tail}
\title{Tail probability for the inner-product error}
\usage{
morie_tqipb_tail(var, eps, norm_sq = 1, x_norm_sq = 1, tail = "chebyshev")
}
\arguments{
\item{var}{Variance bound, from morie_tqipb_variance.}
\item{eps}{Relative accuracy, positive.}
\item{norm_sq}{Squared norm of y.}
\item{x_norm_sq}{Squared norm of x.}
\item{tail}{"chebyshev" (nothing assumed beyond the variance) or   "sub_gaussian" (assumes a sub-Gaussian error with the stated   variance proxy -- an assumption the paper does not make).}
}
\description{
delta such that Pr[|error| > eps ||x|| ||y||] <= delta.
}
\value{
A probability bound, clamped at 1 since a bound above 1 says   nothing.
}
\keyword{internal}
\examples{
morie_tqipb_tail(var = 5L, eps = 0.5)
}
