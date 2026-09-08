% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{fit_stages}
\alias{fit_stages}
\title{Backward induction across K stages, using every subject}
\usage{
fit_stages(stages, propensity = NULL, ridge = 1e-06) 
}
\arguments{
\item{stages}{A list of \code{(R, A, H)} triples, one per stage   in chronological order.}
\item{propensity}{Randomisation probabilities.}
\item{ridge}{Ridge penalty.}
}
\description{
Working backwards, the pseudo-outcome carried into stage k is the reward at k plus the value achieved downstream. For subjects whose later treatment matched the estimated optimum that is their observed future reward, and for the rest it is the model-based prediction from a fresh AOL prognostic fit. That augmentation is what stops the sample from shrinking stage by stage: every subject contributes at every stage.
}
\value{
A list with \code{estimate}, \code{rules},   \code{n_stages}, \code{n_used_per_stage}, \code{n},   \code{method} and \code{note}.
}
\keyword{internal}
\examples{
set.seed(3)
n <- 40
H1 <- matrix(rnorm(n * 2), n, 2)
A1 <- sample(c(-1, 1), n, replace = TRUE)
R1 <- 1 + A1 * H1[, 1] + rnorm(n, 0, 0.3)
H2 <- matrix(rnorm(n * 2), n, 2)
A2 <- sample(c(-1, 1), n, replace = TRUE)
R2 <- 1 + A2 * H2[, 1] + rnorm(n, 0, 0.3)
r <- fit_stages(list(list(R1, A1, H1), list(R2, A2, H2)))
str(r, max.level = 1)
}
