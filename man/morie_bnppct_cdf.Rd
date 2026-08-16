% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_bnppct_cdf}
\alias{morie_bnppct_cdf}
\title{Unnormalised mixture CDF}
\usage{
morie_bnppct_cdf(x, w, mu, s2)
}
\arguments{
\item{x}{The point.}
\item{w}{Component weights.}
\item{mu}{Component means.}
\item{s2}{Component variances.}
}
\description{
Unnormalised mixture CDF
}
\value{
sum_k w_k Phi((x - mu_k)/sqrt(s2_k)).
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
morie_bnppct_cdf(x = 5L, w = c(1, 2, 3, 4, 5, 6, 7, 8), mu = c(1, 2, 3, 4, 5, 6, 7, 8), s2 = c(1, 2, 3, 4, 5, 6, 7, 8))
}
