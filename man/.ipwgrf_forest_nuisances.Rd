% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ipwgrf_forest_nuisances}
\alias{.ipwgrf_forest_nuisances}
\title{.ipwgrf_forest_nuisances}
\usage{
.ipwgrf_forest_nuisances(y, W, X, n_folds = 5, n_trees = 120, min_leaf = 5, seed = 0)
}
\arguments{
\item{y}{See Usage.}
\item{W}{See Usage.}
\item{X}{See Usage.}
\item{n_folds}{Defaults to \code{5}.}
\item{n_trees}{Defaults to \code{120}.}
\item{min_leaf}{Defaults to \code{5}.}
\item{seed}{Defaults to \code{0}.}
}
\description{
Part of the ipwgrf_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{mu1}, \code{mu0}, \code{e}.
}
\references{
----------
Robins, J. M., Rotnitzky, A. & Zhao, L. P. (1994).  The augmented IPW
  score.  JASA 89(427), 846-866, doi:10.1080/01621459.1994.10476818.
Athey, S., Tibshirani, J. & Wager, S. (2019).  Generalized Random
  Forests.  Ann. Statist. 47(2), 1148-1178, doi:10.1214/18-AOS1709.
Chernozhukov, V., Chetverikov, D., Demirer, M., Duflo, E., Hansen,
  C., Newey, W. & Robins, J. (2018).  Double/debiased machine learning
  for treatment and structural parameters.  Econometrics J. 21(1),
  C1-C68, doi:10.1111/ectj.12097.
Crump, R. K., Hotz, V. J., Imbens, G. W. & Mitnik, O. A. (2009).
  Dealing with limited overlap in estimation of average treatment
  effects.  Biometrika 96(1), 187-199, doi:10.1093/biomet/asn055.
}
\keyword{internal}
