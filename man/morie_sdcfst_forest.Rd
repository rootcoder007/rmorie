% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_sdcfst_forest}
\alias{morie_sdcfst_forest}
\title{Grow an honest regression forest on the given rows}
\usage{
morie_sdcfst_forest(X, y, rows, n_trees = 20L, mtry = NULL, min_leaf = 5L, max_depth = 6L, 
    e = NULL) 
}
\arguments{
\item{X}{Covariate matrix.}
\item{y}{Response.}
\item{rows}{Row indices to grow on.}
\item{n_trees}{Number of trees.}
\item{mtry}{Features tried per split; the square root of the column   count by default.}
\item{min_leaf}{Minimum rows in a leaf.}
\item{max_depth}{Maximum depth.}
\item{e}{A generator environment from .ghc_rng.}
}
\description{
Each tree draws a subsample and splits it in HALF: one half chooses the splits, the other fills the leaves. That is the honesty condition -- a leaf's value never sees the points that put it there.
}
\value{
A list of trees plus the settings used.
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
\examples{
morie_sdcfst_forest(X = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2), y = c(1, 2, 3, 4, 5, 6, 7, 8), rows = c(1, 2, 3, 4, 5, 6, 7, 8))
}
