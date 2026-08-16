% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{LogReg}
\alias{LogReg}
\title{Section 10.7, fitted by Newton-Raphson on the log-likelihood.  Unlike}
\usage{
LogReg(X, y, maxiter = 100, tol = 1e-08, ridge = 1e-08)
}
\arguments{
\item{X}{A matrix; passed to \code{as.matrix}.}
\item{y}{See Usage.}
\item{maxiter}{Defaults to \code{100}.}
\item{tol}{Defaults to \code{1e-08}.}
\item{ridge}{Defaults to \code{1e-08}.}
}
\description{
the Bayes classifier it models the POSTERIOR directly and assumes nothing about the shape of p(x|C), which is why it survives features that are plainly not Gaussian.  Perfectly separable classes have no finite maximum, so a small ridge is added and `separable` is reported -- without it the coefficients merely record where the optimizer stopped.
}
\value{
A list with \code{intercept}, \code{coefficients}, \code{w}, \code{fitted}, \code{predicted}, \code{loglik}, \code{iterations}, \code{converged}, \code{separable}, \code{ridge}, \code{training_accuracy}, \code{n}, \code{models_the_posterior_directly}, \code{no_gaussian_assumption}, \code{method}.
}
\keyword{internal}
