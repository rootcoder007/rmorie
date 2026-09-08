% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_plrgrf}
\alias{morie_plrgrf}
\title{morie_plrgrf}
\usage{
morie_plrgrf(y, W, X, at = NULL, n_trees = 200, n_folds = 5, min_leaf = 5, 
    seed = 0, center = TRUE, level = 0.95) 
}
\arguments{
\item{y}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{W}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{X}{A matrix; passed to \code{as.matrix}.}
\item{at}{Optional; may be \code{NULL}. A matrix; passed to \code{as.matrix}.}
\item{n_trees}{Numeric; combined arithmetically in the body. Defaults to \code{200}.}
\item{n_folds}{Passed to \code{local_centering}. Defaults to \code{5}.}
\item{min_leaf}{Passed to \code{local_centering}. Defaults to \code{5}.}
\item{seed}{Passed to \code{local_centering}. Defaults to \code{0}.}
\item{center}{A flag; the body branches on it. Defaults to \code{TRUE}.}
\item{level}{Numeric; combined arithmetically in the body. Defaults to \code{0.95}.}
}
\description{
A step of the plrgrf_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{tau}, \code{se}, \code{ci}, \code{m_hat}, \code{e_hat}, \code{y_residual}, \code{w_residual}, \code{centered}, \code{n}, \code{n_trees}, \code{ate}, \code{level}, \code{method}.
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
fit <- morie_plrgrf(y, W, X, n_trees = 40, n_folds = 3)
is.list(fit)
}
