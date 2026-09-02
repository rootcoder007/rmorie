% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{local_centering}
\alias{local_centering}
\title{local_centering}
\usage{
local_centering(y, W, X, n_folds = 5, n_trees = 100, min_leaf = 5, seed = 0) 
}
\arguments{
\item{y}{A vector; its length is taken.}
\item{W}{Passed to \code{.plrgrf_forest_predict}.}
\item{X}{Passed to \code{.plrgrf_forest_predict}.}
\item{n_folds}{Passed to \code{.plrgrf_folds}. Defaults to \code{5}.}
\item{n_trees}{Passed to \code{.plrgrf_forest_predict}. Defaults to \code{100}.}
\item{min_leaf}{Passed to \code{.plrgrf_forest_predict}. Defaults to \code{5}.}
\item{seed}{Numeric; combined arithmetically in the body. Defaults to \code{0}.}
}
\description{
A step of the plrgrf_native implementation. Called by \code{morie_plrgrf}. See the file header for the source the module follows.
}
\value{
A list with \code{mh}, \code{eh}.
}
\references{
Athey, S., Tibshirani, J. & Wager, S. (2019) "Generalized Random
Forests", The Annals of Statistics 47(2), 1148-1178,
doi:10.1214/18-AOS1709, arXiv:1610.01271. Eq. (2), (3), (8); Sec.
6.1.1 on local centering.

Nie, X. & Wager, S. (2021) "Quasi-oracle estimation of heterogeneous
treatment effects", Biometrika 108(2), 299-319,
doi:10.1093/biomet/asaa076. The R-learner objective this solves
locally.

Robinson, P. M. (1988) "Root-N-Consistent Semiparametric Regression",
Econometrica 56(4), 931-954, doi:10.2307/1912705. The partially
linear model and the residual-on-residual construction.

Chernozhukov, V., Chetverikov, D., Demirer, M., Duflo, E., Hansen, C.,
Newey, W. & Robins, J. (2018) "Double/debiased machine learning for
treatment and structural parameters", The Econometrics Journal 21(1),
C1-C68, doi:10.1111/ectj.12097. Neyman orthogonality and cross-fitting.
}
\keyword{internal}
\examples{
set.seed(1)
n <- 120
X <- matrix(rnorm(n * 3), n, 3)
W <- rbinom(n, 1, 0.5)
y <- W * (1 + X[, 1]) + X[, 2] + rnorm(n, 0, 0.5)
lc <- local_centering(y, W, X, n_folds = 3, n_trees = 40)
c(length(lc$mh), length(lc$eh))
}
