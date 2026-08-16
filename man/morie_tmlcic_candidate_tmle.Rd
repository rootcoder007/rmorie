% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_tmlcic_candidate_tmle}
\alias{morie_tmlcic_candidate_tmle}
\title{morie_tmlcic_candidate_tmle}
\usage{
morie_tmlcic_candidate_tmle(y, A, W, cand, g1, rows = NULL, eval_rows = NULL, ridge = 1e-08, target_step = TRUE)
}
\arguments{
\item{y}{See Usage.}
\item{A}{See Usage.}
\item{W}{See Usage.}
\item{cand}{See Usage.}
\item{g1}{See Usage.}
\item{rows}{Defaults to \code{NULL}.}
\item{eval_rows}{Defaults to \code{NULL}.}
\item{ridge}{Defaults to \code{1e-08}.}
\item{target_step}{Defaults to \code{TRUE}.}
}
\description{
Part of the tmlcic_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{q1}, \code{q0}, \code{qa}, \code{info}.
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
