% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_tmlcic_influence_curve}
\alias{morie_tmlcic_influence_curve}
\title{morie_tmlcic_influence_curve}
\usage{
morie_tmlcic_influence_curve(y, A, q1, q0, qa, gA, rows, psi, target) 
}
\arguments{
\item{y}{A vector; its length is taken and its elements indexed.}
\item{A}{A vector; indexed elementwise.}
\item{q1}{A vector; indexed elementwise.}
\item{q0}{A vector; indexed elementwise.}
\item{qa}{A vector; indexed elementwise.}
\item{gA}{A vector; indexed elementwise.}
\item{rows}{See Usage.}
\item{psi}{Numeric; combined arithmetically in the body.}
\item{target}{Compared against \code{"SATE"}.}
}
\description{
A step of the tmlcic_native implementation. Called by \code{morie_tmlcic_adaptive_prespecification}, \code{morie_tmlcic_tmle_cluster_ic}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
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
set.seed(1)
n <- 40
W <- matrix(rnorm(n * 2), n, 2)
A <- rbinom(n, 1, 0.5)
y <- plogis(-0.3 + 0.8 * A + 0.5 * W[, 1] + rnorm(n, 0, 0.5))
cand <- list(name = "W1", cols = 0L, interact = FALSE)
ct <- morie_tmlcic_candidate_tmle(y, A, W, cand, g1 = function(i) 0.5)
psi <- mean(ct$q1 - ct$q0)
D <- morie_tmlcic_influence_curve(y, A, ct$q1, ct$q0, ct$qa,
                                  rep(0.5, n), seq_len(n), psi, "SATE")
head(D)
}
