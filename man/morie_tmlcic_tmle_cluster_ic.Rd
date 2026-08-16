% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_tmlcic_tmle_cluster_ic}
\alias{morie_tmlcic_tmle_cluster_ic}
\title{morie_tmlcic_tmle_cluster_ic}
\usage{
morie_tmlcic_tmle_cluster_ic(y, D, X, cluster = NULL, target = "SATE", design = NULL, library = NULL, g_library = NULL, n_folds = NULL, adapt = TRUE, ridge = 1e-08, level = 0.95)
}
\arguments{
\item{y}{Passed to \code{.s03vec}.}
\item{D}{Passed to \code{.s03vec}.}
\item{X}{Optional; may be \code{NULL}. Passed to \code{.s03mat}.}
\item{cluster}{Optional; may be \code{NULL}. Passed to \code{.tmlcic_pairs_from}.}
\item{target}{Passed to \code{morie_tmlcic_adaptive_prespecification}. Defaults to \code{"SATE"}.}
\item{design}{Optional; may be \code{NULL}. One of \code{"matched"}, \code{"unmatched"}.}
\item{library}{Passed to \code{morie_tmlcic_adaptive_prespecification}.}
\item{g_library}{Passed to \code{morie_tmlcic_adaptive_prespecification}.}
\item{n_folds}{Passed to \code{morie_tmlcic_adaptive_prespecification}.}
\item{adapt}{A flag; the body branches on it. Defaults to \code{TRUE}.}
\item{ridge}{Passed to \code{morie_tmlcic_adaptive_prespecification}. Defaults to \code{1e-08}.}
\item{level}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.95}.}
}
\description{
A step of the tmlcic_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{se}, \code{n}, \code{ci}, \code{level}, \code{unadjusted}, \code{se_unadjusted}, \code{variance_ratio}, \code{q_selected}, \code{q_risks}, \code{g_selected}, \code{g_risks}, \code{epsilon}, \code{influence_curve}, \code{eic_mean}, \code{rho}, \code{independent_units}, \code{unit}, \code{design}, \code{target}, \code{n_folds}, \code{adapt}, \code{method}.
}
\references{
----------
Balzer, L. B., van der Laan, M. J. & Petersen, M. L. (2018)
"Data-Adaptive Estimation in Cluster Randomized Trials", Ch. 13 in
Targeted Learning in Data Science, Springer, pp. 195-215,
doi:10.1007/978-3-319-65304-4_13.

Balzer, L. B., van der Laan, M. J. & Petersen, M. L. (2016) "Adaptive
pre-specification in randomized trials with and without
pair-matching", Statistics in Medicine 35(25), 4528-4545,
doi:10.1002/sim.7023.

Balzer, L. B., Zheng, W., van der Laan, M. J. & Petersen, M. L.
(2019) "A new approach to hierarchical data analysis", Statistical
Methods in Medical Research 28(6), 1761-1780,
doi:10.1177/0962280218774936.
}
\keyword{internal}
