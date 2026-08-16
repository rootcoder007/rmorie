% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_geron_policy}
\alias{morie_geron_policy}
\title{Policy pi(a|s): probabilities, entropy and a reproducible sampled action (Geron Ch 19, hmpol)}
\usage{
morie_geron_policy(state, pi, seed = 0)
}
\arguments{
\item{state}{Current state (0-based).}
\item{pi}{Function, matrix, vector or named list.}
\item{seed}{Integer-LCG seed for the sampled action.}
}
\description{
Policy pi(a|s): probabilities, entropy and a reproducible sampled action (Geron Ch 19, hmpol)
}
\value{
A list with \code{probabilities}, \code{action}, \code{greedy_action}, \code{entropy}, \code{deterministic}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_geron_policy(V, V)
}
