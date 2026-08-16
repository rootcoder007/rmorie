% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tmlcic_cv_folds}
\alias{.tmlcic_cv_folds}
\title{Folds that respect the pairing: a pair is never split}
\usage{
.tmlcic_cv_folds(groups, n_folds, design, n)
}
\arguments{
\item{groups}{See Usage.}
\item{n_folds}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
\item{design}{Compared against \code{"unmatched"}.}
\item{n}{A count; the body uses it as \code{seq_len(...)}.}
}
\description{
A step of the tmlcic_native implementation. Called by \code{morie_tmlcic_adaptive_prespecification}. See the file header for the source the module follows.
}
\value{
The value of \code{Filter}.
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
