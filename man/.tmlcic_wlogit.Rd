% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tmlcic_wlogit}
\alias{.tmlcic_wlogit}
\title{Weighted logistic IRLS with a ridge penalty}
\usage{
.tmlcic_wlogit(X, y, ridge = 1e-10, obs_weights = NULL)
}
\arguments{
\item{X}{A matrix; passed to \code{nrow}.}
\item{y}{Numeric; combined arithmetically in the body.}
\item{ridge}{Numeric; passed to \code{max}. Defaults to \code{1e-10}.}
\item{obs_weights}{Defaults to \code{NULL}.}
}
\description{
A step of the tmlcic_native implementation. Called by \code{.tmlcic_fit_g}, \code{.tmlcic_fit_working_model}, \code{.tmlcic_hier_cluster_arm} and 1 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{beta}, as built in the body.
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
