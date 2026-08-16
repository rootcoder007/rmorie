% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_tmlcic_variance_estimate}
\alias{morie_tmlcic_variance_estimate}
\title{morie_tmlcic_variance_estimate}
\usage{
morie_tmlcic_variance_estimate(D, y, qa, groups, n, design, target)
}
\arguments{
\item{D}{A vector; indexed elementwise.}
\item{y}{A vector; indexed elementwise.}
\item{qa}{A vector; indexed elementwise.}
\item{groups}{A vector; its length is taken.}
\item{n}{A count; the body uses it as \code{seq_len(...)}.}
\item{design}{Compared against \code{"unmatched"}.}
\item{target}{Compared against \code{"PATE"}.}
}
\description{
A step of the tmlcic_native implementation. Called by \code{morie_tmlcic_tmle_cluster_ic}. See the file header for the source the module follows.
}
\value{
A list with \code{var}, \code{info}.
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
set.seed(2)
n <- 40
D <- rnorm(n, 0, 0.6)
y <- runif(n)
qa <- runif(n)
groups <- split(seq_len(n), rep(1:8, each = 5))
r <- morie_tmlcic_variance_estimate(D, y, qa, groups, n,
                                    design = "unmatched", target = "SATE")
str(r, max.level = 1)
}
