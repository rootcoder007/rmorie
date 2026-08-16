% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_bnppct_expand}
\alias{morie_bnppct_expand}
\title{Widen a bracket by doubling until f changes sign across it}
\usage{
morie_bnppct_expand(f, lo, hi, iters = 60L)
}
\arguments{
\item{f}{The function.}
\item{lo}{Lower end.}
\item{hi}{Upper end.}
\item{iters}{Maximum doublings.}
}
\description{
A fixed doubling schedule, not a search: both arms then visit exactly the same endpoints and the bisection that follows starts from the same interval.
}
\value{
A list with the widened lo and hi.
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
