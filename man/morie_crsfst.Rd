% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_crsfst}
\alias{morie_crsfst}
\title{Cross-fitted treatment effect on restricted mean survival time}
\usage{
morie_crsfst(time, event, D, X, K = 3L, tau = NULL, n_trees = 8L, min_leaf = 3L, 
    max_depth = 3L, honest = TRUE, seed = 0, rule = "logrank") 
}
\arguments{
\item{time}{Observed time.}
\item{event}{Event indicator.}
\item{D}{Treatment, zero or one.}
\item{X}{Covariates.}
\item{K}{Folds.}
\item{tau}{The horizon; defaults to the largest time observed in BOTH   arms, because an RMST beyond the point where one arm has any data   left is an extrapolation dressed as an estimate.}
\item{n_trees}{Trees per forest.}
\item{min_leaf}{Minimum leaf size.}
\item{max_depth}{Maximum depth.}
\item{honest}{Whether the trees are honest.}
\item{seed}{The random stream.}
\item{rule}{The survival split rule.}
}
\description{
Cross-fitted treatment effect on restricted mean survival time
}
\value{
A list with the per-observation conditional effect, the   average effect, the two arms' restricted means, the fold   assignment, and the leakage count -- which must be zero.
}
\references{
  Cui, Y., Kosorok, M.R., Sverdrup, E., Wager, S. and Zhu, R. (2023)
    "Estimating heterogeneous treatment effects with right-censored
    data via causal survival forests." Journal of the Royal
    Statistical Society Series B 85(2), 179-211.
    doi:10.1093/jrsssb/qkac001.
  Chernozhukov, V., Chetverikov, D., Demirer, M., Duflo, E., Hansen,
    C., Newey, W. and Robins, J. (2018) "Double/debiased machine
    learning for treatment and structural parameters." The
    Econometrics Journal 21(1), C1-C68.
  Royston, P. and Parmar, M.K.B. (2013) "Restricted mean survival
    time: an alternative to the hazard ratio." BMC Medical Research
    Methodology 13, 152.
  Uno, H. et al. (2014) "Moving beyond the hazard ratio in quantifying
    the clinical benefit-risk of therapies." Journal of Clinical
    Oncology 32(22), 2380-2385.
}
\keyword{internal}
\examples{
N <- 36L
X <- matrix(0, N, 2)
D <- vapply(0:(N - 1L), function(i) if (i\%\%2 == 1) 1L else 0L, 
    integer(1))
T <- vapply(0:(N - 1L), function(i) round(3 + 6 * (1 - ((i * 
    7)\%\%11)/10) + 4 * (if (i\%\%2 == 1) 1 else 0) + ((i * 3)\%\%5) * 
    0.3, 4), numeric(1))
E <- vapply(0:(N - 1L), function(i) if (i\%\%5 == 0) 0L else 1L, 
    integer(1))
morie_crsfst(T, E, D, X, 3L, NULL, 4L, 3L, 2L, TRUE, 2)
}
