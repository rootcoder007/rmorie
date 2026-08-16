% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_tmlcic_cluster_weights}
\alias{morie_tmlcic_cluster_weights}
\title{The per-individual weights alpha_ij and the cluster groups. Balzer}
\usage{
morie_tmlcic_cluster_weights(cluster, weights = NULL)
}
\arguments{
\item{cluster}{See Usage.}
\item{weights}{Defaults to \code{NULL}.}
}
\description{
et al. (2019) require sum_i alpha_ij = 1 within each cluster. The default alpha_ij = 1/N_j is their stated choice.
}
\value{
A list with \code{alpha}, \code{groups}.
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
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_tmlcic_cluster_weights(V)
}
