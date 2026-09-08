% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_qbcfgs_smd}
\alias{morie_qbcfgs_smd}
\title{Standardised mean difference of one covariate between arms}
\usage{
morie_qbcfgs_smd(x, d, w = NULL) 
}
\arguments{
\item{x}{One covariate.}
\item{d}{Treatment indicators.}
\item{w}{Weights, or NULL for unweighted.}
}
\description{
The difference in means over the pooled standard deviation, so it is unit-free and comparable across covariates. With weights it is the weighted version, which is the number a weighting scheme is claiming to shrink.
}
\value{
The standardised mean difference.
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
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_qbcfgs_smd(V, V)
}
