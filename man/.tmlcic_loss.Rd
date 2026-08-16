% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tmlcic_loss}
\alias{.tmlcic_loss}
\title{Eq. (13.5)/(13.6) unmatched, (13.8)/(13.9) matched}
\usage{
.tmlcic_loss(D, y, qa, groups, design, target, rows)
}
\arguments{
\item{D}{A vector; indexed elementwise.}
\item{y}{A vector; indexed elementwise.}
\item{qa}{A vector; indexed elementwise.}
\item{groups}{See Usage.}
\item{design}{Compared against \code{"unmatched"}.}
\item{target}{Compared against \code{"PATE"}.}
\item{rows}{A vector; its length is taken.}
}
\description{
A step of the tmlcic_native implementation. Called by \code{morie_tmlcic_adaptive_prespecification}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
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
