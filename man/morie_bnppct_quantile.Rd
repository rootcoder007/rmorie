% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_bnppct_quantile}
\alias{morie_bnppct_quantile}
\title{Invert the mixture CDF at q, normalising by the carried mass}
\usage{
morie_bnppct_quantile(q, w, mu, s2, lo = NULL, hi = NULL) 
}
\arguments{
\item{q}{The probability.}
\item{w}{Component weights.}
\item{mu}{Component means.}
\item{s2}{Component variances.}
\item{lo}{Lower end of the bracket, or NULL to take it from the   components.}
\item{hi}{Upper end of the bracket, or NULL.}
}
\description{
Bisection rather than Newton: the CDF is monotone but its derivative is a mixture of narrow normals, and a Newton step off a flat stretch between two well-separated components lands anywhere. The bracket is taken from the COMPONENTS, not from the data. A slice sampler instantiates components from the prior to cover the slice, and an inverse-gamma prior draw can be enormous; such a component leaves the CDF far short of one anywhere near the data, so a data-width bracket fails to contain the root. That is not a numerical nuisance -- it is the model saying this draw of F has a very heavy tail -- so the bracket follows the components and is then widened until it genuinely brackets.
}
\value{
The quantile.
}
\references{
  Kottas, A. and Krnjajic, M. (2009) "Bayesian semiparametric
    modelling in quantile regression." Scandinavian Journal of
    Statistics 36(2), 297-319.
  Rubin, D.B. (1981) "The Bayesian bootstrap." Annals of Statistics
    9(1), 130-134.
  Walker (2007); Kalli, Griffin and Walker (2011) -- the sampler,
    through morie_slbpdg.
}
\keyword{internal}
\examples{
morie_bnppct_quantile(q = 0.5, w = c(1, 2, 3, 4, 5, 6, 7, 8), mu = c(1, 2, 3, 4, 5, 6, 7, 8), s2 = c(1, 2, 3, 4, 5, 6, 7, 8))
}
