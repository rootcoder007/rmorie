% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{nb_glm_fit}
\alias{nb_glm_fit}
\title{Negative binomial GLM by ridge-penalised IRLS}
\usage{
nb_glm_fit(K, X, alpha, s = NULL, lam = NULL, max_iter = 100L, tol = 1e-08, beta0 = NULL)
}
\arguments{
\item{K}{Sample-by-1 response vector.}
\item{X}{Design matrix.}
\item{alpha}{Dispersion.}
\item{s}{Optional size factors.}
\item{lam}{Optional ridge vector (one per coefficient).}
\item{max_iter}{Maximum iterations.}
\item{tol}{Convergence tolerance on the parameter step.}
\item{beta0}{Optional starting coefficients.}
}
\description{
Update is the paper\'s own: \code{beta <- (X\' W X + lambda I)^{-1} X\' W z} with \code{z_j = log(mu_j / s_j) + (K_j - mu_j) / mu_j} and \code{w_jj = 1 / (1/mu_j + alpha)}.
}
\value{
A list with \code{beta}, \code{mu}, \code{sigma},   \code{converged}, \code{n_iter}.
}
\keyword{internal}
