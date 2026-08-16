% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_qbcfgs}
\alias{morie_qbcfgs}
\title{A stratum-balanced treatment effect and its balance table}
\usage{
morie_qbcfgs(y, D, X, quantile = 0.5, n_strata = 4L, weight = "ate", n_trees = 8L, min_leaf = 3L, max_depth = 3L, seed = 0, clip = 0.01)
}
\arguments{
\item{y}{Outcome.}
\item{D}{Binary treatment.}
\item{X}{Covariates.}
\item{quantile}{Which propensity quantile the focal stratum is read   at.}
\item{n_strata}{Number of equal-size strata.}
\item{weight}{"ate" weights each unit towards the whole population,   "att" towards the treated.}
\item{n_trees}{Trees in the propensity forest.}
\item{min_leaf}{Minimum leaf size.}
\item{max_depth}{Maximum depth.}
\item{seed}{The random stream.}
\item{clip}{The propensity is held inside the interval before it   enters a denominator. Reported, not silent.}
}
\description{
A stratum-balanced treatment effect and its balance table
}
\value{
A list with the per-stratum effects and sizes, the overall   effect, the balance before and after weighting, and the focal   stratum.
}
\references{
  Hsu, Y.-C., Huber, M., Lee, Y.-Y. and Pipoz, L. (2022) "Direct and
    indirect effects of continuous treatments based on generalized
    propensity score weighting." Journal of Applied Econometrics
    37(2), 449-460.
  Rosenbaum, P.R. and Rubin, D.B. (1983) "The central role of the
    propensity score in observational studies for causal effects."
    Biometrika 70(1), 41-55.
  Rosenbaum, P.R. and Rubin, D.B. (1984) "Reducing bias in
    observational studies using subclassification on the propensity
    score." Journal of the American Statistical Association 79(387),
    516-524.
  Austin, P.C. (2009) "Balance diagnostics for comparing the
    distribution of baseline covariates between treatment groups in
    propensity-score matched samples." Statistics in Medicine 28(25),
    3083-3107.
  Wager, S. and Athey, S. (2018) "Estimation and inference of
    heterogeneous treatment effects using random forests." JASA
    113(523), 1228-1242.
}
\keyword{internal}
