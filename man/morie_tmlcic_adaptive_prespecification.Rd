% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_tmlcic_adaptive_prespecification}
\alias{morie_tmlcic_adaptive_prespecification}
\title{morie_tmlcic_adaptive_prespecification}
\usage{
morie_tmlcic_adaptive_prespecification(y, A, W, groups, design, target, library = NULL, g_library = NULL, 
    n_folds = NULL, ridge = 1e-08) 
}
\arguments{
\item{y}{A vector; its length is taken.}
\item{A}{Passed to \code{morie_tmlcic_candidate_tmle}.}
\item{W}{A matrix; passed to \code{ncol}.}
\item{groups}{Passed to \code{.tmlcic_cv_folds}.}
\item{design}{Passed to \code{.tmlcic_cv_folds}.}
\item{target}{Passed to \code{morie_tmlcic_influence_curve}.}
\item{library}{Optional; may be \code{NULL}. Passed to \code{is.null}.}
\item{g_library}{Optional; may be \code{NULL}. Passed to \code{is.null}.}
\item{n_folds}{Passed to \code{.tmlcic_cv_folds}.}
\item{ridge}{Passed to \code{morie_tmlcic_candidate_tmle}. Defaults to \code{1e-08}.}
}
\description{
A step of the tmlcic_native implementation. Called by \code{morie_tmlcic_tmle_cluster_ic}. See the file header for the source the module follows.
}
\value{
A list with \code{q_candidate}, \code{q_risks}, \code{q_names}, \code{g_candidate}, \code{g_risks}, \code{g_names}, \code{gfit}, \code{n_folds}.
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
\examples{
set.seed(3)
n <- 40
W <- matrix(rnorm(n * 2), n, 2)
A <- rep(c(0, 1), 20)
y <- plogis(-0.3 + 0.8 * A + 0.5 * W[, 1] + rnorm(n, 0, 0.5))
groups <- split(seq_len(n), rep(1:8, each = 5))
r <- morie_tmlcic_adaptive_prespecification(y, A, W, groups,
                                            design = "unmatched",
                                            target = "SATE")
str(r, max.level = 1)
}
