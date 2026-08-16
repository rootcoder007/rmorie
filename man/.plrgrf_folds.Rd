% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.plrgrf_folds}
\alias{.plrgrf_folds}
\title{.plrgrf_folds}
\usage{
.plrgrf_folds(n, V)
}
\arguments{
\item{n}{See Usage.}
\item{V}{See Usage.}
}
\description{
Part of the plrgrf_native implementation; see the file header for the source it follows.
}
\value{
The value of \code{lapply}.
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
