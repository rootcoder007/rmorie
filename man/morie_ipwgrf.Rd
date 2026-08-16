% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_ipwgrf}
\alias{morie_ipwgrf}
\title{morie_ipwgrf}
\usage{
morie_ipwgrf(y, W, X, n_folds = 5, n_trees = 120, min_leaf = 5, trim = 0.02, seed = 0, level = 0.95, break_outcome = FALSE, break_propensity = FALSE)
}
\arguments{
\item{y}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{W}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{X}{A matrix; passed to \code{as.matrix}.}
\item{n_folds}{Passed to \code{.ipwgrf_forest_nuisances}. Defaults to \code{5}.}
\item{n_trees}{Passed to \code{.ipwgrf_forest_nuisances}. Defaults to \code{120}.}
\item{min_leaf}{Passed to \code{.ipwgrf_forest_nuisances}. Defaults to \code{5}.}
\item{trim}{Passed to \code{.ipwgrf_aipw_scores}. Defaults to \code{0.02}.}
\item{seed}{Passed to \code{.ipwgrf_forest_nuisances}. Defaults to \code{0}.}
\item{level}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.95}.}
\item{break_outcome}{A flag; the body branches on it. Defaults to \code{FALSE}.}
\item{break_propensity}{A flag; the body branches on it. Defaults to \code{FALSE}.}
}
\description{
A step of the ipwgrf_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{ate}, \code{se}, \code{ci}, \code{scores}, \code{mu1}, \code{mu0}, \code{propensity}, \code{plug_in}, \code{max_weight}, \code{min_propensity}, \code{max_propensity}, \code{trim}, \code{n}, \code{level}, \code{broken_outcome}, \code{broken_propensity}, \code{method}.
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
