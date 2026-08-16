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
\item{y}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{maxiter}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{100}.}
\item{tol}{Defaults to \code{1e-08}.}
\item{ridge}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1e-08}.}
}
\description{
the Bayes classifier it models the POSTERIOR directly and assumes nothing about the shape of p(x|C), which is why it survives features that are plainly not Gaussian.  Perfectly separable classes have no finite maximum, so a small ridge is added and `separable` is reported -- without it the coefficients merely record where the optimizer stopped.
}
\value{
A list with \code{intercept}, \code{coefficients}, \code{w}, \code{fitted}, \code{predicted}, \code{loglik}, \code{iterations}, \code{converged}, \code{separable}, \code{ridge}, \code{training_accuracy}, \code{n}, \code{models_the_posterior_directly}, \code{no_gaussian_assumption}, \code{method}.
}
\keyword{internal}
\examples{
XC <- matrix(c(1, 1, 1.4, 0.8, 0.7, 1.3, 1.1, 1.6, 3, 3, 3.3, 
    2.7, 2.6, 3.2, 3.1, 3.5), ncol = 2, byrow = TRUE)
YC <- c(0, 0, 0, 0, 1, 1, 1, 1)
LogReg(XC, YC)
}
