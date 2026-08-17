% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{optimal_alpha}
\alias{optimal_alpha}
\title{Theorem 5.2: the variance-minimising trimming threshold}
\usage{
optimal_alpha(pscore, sigma2_treated = NULL, sigma2_control = NULL, tol = 1e-12, max_iter = 200)
}
\arguments{
\item{pscore}{Propensity scores.}
\item{sigma2_treated}{Optional conditional variance under treatment.}
\item{sigma2_control}{Optional conditional variance under control.}
\item{tol}{Convergence tolerance.}
\item{max_iter}{Maximum iterations.}
}
\description{
Solves gamma = 2 E[k(X) | k(X) < gamma] by the fixed-point iteration the equation itself defines, starting from the no-trimming point. With homoskedastic variances (the default) k(x) = 1 / (e (1 - e)).
}
\value{
A list with \code{alpha}, \code{gamma}, \code{keep},   \code{trim}, \code{no_trimming}, \code{k}.
}
\keyword{internal}
\examples{
set.seed(1)
e <- runif(100, 0.02, 0.98)
str(optimal_alpha(e), max.level = 1)
}
