% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_sdcfst}
\alias{morie_sdcfst}
\title{Cross-fitted treatment effect with honest-forest nuisances}
\usage{
morie_sdcfst(y, D, X, K_fold = 5L, score = "aipw", learner = "forest", n_trees = 20L, mtry = NULL, min_leaf = 5L, max_depth = 6L, trim = 0.02, seed = 1, ridge = 1e-06)
}
\arguments{
\item{y}{Outcome.}
\item{D}{Binary treatment, 0 or 1.}
\item{X}{Covariates.}
\item{K_fold}{Number of cross-fitting folds. K = 1 means no   cross-fitting at all, offered so the bias it removes can be   measured.}
\item{score}{"aipw", "partialling_out", "ipw" or "plugin".}
\item{learner}{"forest" or "linear".}
\item{n_trees}{Trees per forest.}
\item{mtry}{Features tried per split.}
\item{min_leaf}{Minimum rows in a leaf.}
\item{max_depth}{Maximum tree depth.}
\item{trim}{Propensities are clipped into this range from each end,   and the number clipped is reported.}
\item{seed}{Seed for the generator shared with the Python arm.}
\item{ridge}{Ridge penalty for the logistic propensity.}
}
\description{
Cross-fitted treatment effect with honest-forest nuisances
}
\value{
A list with the estimate, its standard error from the   influence function, the per-fold estimates, the trimming count and   the fitted propensity summary.
}
\references{
  Chernozhukov, V., Chetverikov, D., Demirer, M., Duflo, E., Hansen,
    C., Newey, W. and Robins, J. (2018) "Double/debiased machine
    learning for treatment and structural parameters." The
    Econometrics Journal 21(1), C1-C68.
  Robinson, P.M. (1988) "Root-N-consistent semiparametric regression."
    Econometrica 56(4), 931-954.
  Robins, J.M., Rotnitzky, A. and Zhao, L.P. (1994) JASA 89(427),
    846-866.
  Wager, S. and Athey, S. (2018) "Estimation and inference of
    heterogeneous treatment effects using random forests." JASA
    113(523), 1228-1242.
}
\keyword{internal}
