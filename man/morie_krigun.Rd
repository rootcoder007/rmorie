% Generated from the module source by ledger/gen_rd.R -- roxygen2 hangs
% on this package. Usage comes from args(), so it tracks the formals.
\name{morie_krigun}
\alias{morie_krigun}
\title{R arm of morie/fn/krigun.py -- universal kriging with a polynomial trend}
\usage{
morie_krigun(coords, values, s_predict, trend_order = 1, model = "exponential", nugget = 0, sill = 1, range_ = 1)
}
\description{
R arm of morie/fn/krigun.py -- universal kriging with a polynomial trend. The Python body was a placeholder: it averaged `coords` and used neither `values`, `s_predict` nor `trend_order`. There was no R arm at all.   Z(s) = mu(s) + delta(s),  mu(s) = sum_k beta_k f_k(s) with delta zero-mean second-order stationary. The predictor solves the augmented system that enforces unbiasedness on every trend basis function, so the trend coefficients never have to be estimated separately. Order 0 makes the basis a single column of ones, which is exactly the ordinary kriging constraint sum_i lambda_i = 1. ' @noRd
}
\value{
A list with components \code{estimate}, \code{se}, \code{n}, \code{trend_order}, \code{method}.
}
\references{
Cressie (1993) sec. 3.4.5; Schabenberger & Gotway (2005) ch. 5.
}
\keyword{internal}
