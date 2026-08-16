% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tmlcic_hier_cluster_arm}
\alias{.tmlcic_hier_cluster_arm}
\title{.tmlcic_hier_cluster_arm}
\usage{
.tmlcic_hier_cluster_arm(yc, Aj, Zj, groups, a, trim, ridge, known_g)
}
\arguments{
\item{yc}{Numeric; combined arithmetically in the body.}
\item{Aj}{A vector; indexed elementwise.}
\item{Zj}{A matrix; passed to \code{as.matrix}.}
\item{groups}{A vector; its length is taken.}
\item{a}{See Usage.}
\item{trim}{Numeric; combined arithmetically in the body.}
\item{ridge}{Numeric; passed to \code{max}.}
\item{known_g}{See Usage.}
}
\description{
A step of the tmlcic_native implementation. Called by \code{morie_tmlcic_tmle_hierarchical}. See the file header for the source the module follows.
}
\value{
A list with \code{psi}, \code{D}, \code{info}.
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
