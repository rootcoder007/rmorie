% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{CramerRao}
\alias{CramerRao}
\title{\\' Cramer-Rao lower bound implied by a Fisher information matrix}
\usage{
CramerRao(fisher_info, var_estimate = NULL) 
}
\arguments{
\item{fisher_info}{A matrix; passed to \code{nrow}.}
\item{var_estimate}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
\\' \\' Cov(theta_hat) >= I(theta)^-1 in the Loewner order, so the bound on \\' component k is the k-th diagonal entry of the inverse information. \\' \\' @param fisher_info a scalar, a vector read as a diagonal, or a \\' square symmetric matrix \\' @param var_estimate optional actual variances, one per parameter \\' @return list(bound, variance, se, information, k, efficiency, \\' attained, method) \\'
}
\value{
A list with \code{bound}, \code{variance}, \code{se}, \code{information}, \code{k}, \code{efficiency}, \code{attained}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
CramerRao(V)
}
