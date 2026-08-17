% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_mqtmpl_permutation_threshold}
\alias{morie_mqtmpl_permutation_threshold}
\title{Permutation threshold}
\usage{
morie_mqtmpl_permutation_threshold(y, markers, positions, n_perm = 100, alpha = 0.05, method = "em", step = 0.05, seed = 0)
}
\arguments{
\item{y,markers,positions,method,step}{As in \code{scanone}.}
\item{n_perm}{Number of permutations.}
\item{alpha}{Significance level.}
\item{seed}{Seed.}
}
\description{
Permutation threshold
}
\value{
A list with threshold, null maxima, median, alpha, n_perm.
}
\keyword{internal}
\examples{
set.seed(2)
n <- 40
m1 <- rbinom(n, 1, 0.5)
m2 <- ifelse(runif(n) < 0.9, m1, 1 - m1)
y <- 1 + 0.8 * m1 + rnorm(n, 0, 0.5)
r <- morie_mqtmpl_permutation_threshold(y, list(m1, m2),
                                        positions = c(0, 0.1),
                                        n_perm = 20, method = "mr")
str(r, max.level = 1)
}
