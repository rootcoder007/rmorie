% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_interval_map}
\alias{morie_interval_map}
\title{Interval mapping by EM on the mixture likelihood}
\usage{
morie_interval_map(y, left, right, r_left, r_right, max_iter = 200L, tol = 1e-10)
}
\arguments{
\item{y}{Numeric phenotype vector.}
\item{left}{Numeric vector of left-marker genotypes.}
\item{right}{Numeric vector of right-marker genotypes.}
\item{r_left}{Recombination fraction on the left interval.}
\item{r_right}{Recombination fraction on the right interval.}
\item{max_iter}{Maximum number of EM iterations (default 200).}
\item{tol}{Convergence tolerance on successive log-likelihoods   (default 1e-10).}
}
\description{
Implements the EM algorithm of Lander & Botstein (1989) for the two-component mixture likelihood (7) at one QTL position; the E step is the posterior QTL genotype probability given the current parameters, the M step is a weighted regression, and the log-likelihood is recorded at every iteration so the monotone increase is visible.
}
\value{
A named list with keys \code{estimate}, \code{lod},   \code{a}, \code{b}, \code{sigma2}, \code{loglik},   \code{loglik_null}, \code{iterations}, \code{loglik_history},   \code{posterior}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
set.seed(1)
n <- 50
g1 <- rbinom(n, 1, 0.5)
g2 <- ifelse(runif(n) < 0.85, g1, 1 - g1)
y <- 1 + 0.9 * g1 + rnorm(n, 0, 0.5)
r <- morie_interval_map(y, g1, g2, r_left = 0.075, r_right = 0.075)
str(r, max.level = 1)
}
