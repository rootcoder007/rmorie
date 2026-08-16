% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_qbcfgs_strata}
\alias{morie_qbcfgs_strata}
\title{Equal-size strata by the rank of the fitted propensity}
\usage{
morie_qbcfgs_strata(e, n_strata)
}
\arguments{
\item{e}{Fitted propensities.}
\item{n_strata}{Number of strata.}
}
\description{
Ranks rather than values, so a propensity distribution piled up at one end still gives strata of equal size -- which is the point of stratifying by quantile rather than by a grid.
}
\value{
A zero-based stratum index per observation.
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
