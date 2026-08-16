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
