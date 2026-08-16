% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_qsfrgr}
\alias{morie_qsfrgr}
\title{Conditional survival quantiles from a forest}
\usage{
morie_qsfrgr(time, event, X, quantile = 0.5, n_trees = 20L, mtry = NULL, min_leaf = 3L, max_depth = 6L, honest = TRUE, seed = 0, rule = "logrank", newX = NULL, grid = NULL)
}
\arguments{
\item{time}{Observed time.}
\item{event}{Event indicator, one is a death and zero a censoring.}
\item{X}{Covariates, one row per observation.}
\item{quantile}{Which conditional quantile of the survival   distribution.}
\item{n_trees}{Number of trees.}
\item{mtry}{Features tried per split, or NULL.}
\item{min_leaf}{Minimum leaf size.}
\item{max_depth}{Maximum depth.}
\item{honest}{Whether to split the subsample.}
\item{seed}{The random stream.}
\item{rule}{A member of the split list.}
\item{newX}{Points to predict at, or NULL for the training rows.}
\item{grid}{Times at which the conditional survival curve is   reported, or NULL.}
}
\description{
Conditional survival quantiles from a forest
}
\value{
A list with the conditional quantile at each query point, the   survival curve on the grid, the weights' effective sample size, and   how many observations were censored.
}
\references{
  Cui, Y., Kosorok, M.R., Sverdrup, E., Wager, S. and Zhu, R. (2023)
    "Estimating heterogeneous treatment effects with right-censored
    data via causal survival forests." Journal of the Royal
    Statistical Society Series B 85(2), 179-211.
    doi:10.1093/jrsssb/qkac001.
  Wager, S. and Athey, S. (2018) "Estimation and inference of
    heterogeneous treatment effects using random forests." Journal of
    the American Statistical Association 113(523), 1228-1242.
  Athey, S., Tibshirani, J. and Wager, S. (2019) "Generalized random
    forests." The Annals of Statistics 47(2), 1148-1178.
  Ishwaran, H., Kogalur, U.B., Blackstone, E.H. and Lauer, M.S. (2008)
    "Random survival forests." The Annals of Applied Statistics 2(3),
    841-860.
  Kaplan, E.L. and Meier, P. (1958) "Nonparametric estimation from
    incomplete observations." Journal of the American Statistical
    Association 53(282), 457-481.
  Meinshausen, N. (2006) "Quantile regression forests." Journal of
    Machine Learning Research 7, 983-999.
}
\keyword{internal}
