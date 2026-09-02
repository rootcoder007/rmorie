% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{IcaClean}
\alias{IcaClean}
\title{IcaClean}
\usage{
IcaClean(X, ncomp = NULL, kurtosis = 3, drop = NULL, maxiter = 200, seed = 1) 
}
\arguments{
\item{X}{Passed to \code{IcaFix}.}
\item{ncomp}{Passed to \code{IcaFix}.}
\item{kurtosis}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{3}.}
\item{drop}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
\item{maxiter}{Passed to \code{IcaFix}. Defaults to \code{200}.}
\item{seed}{Passed to \code{IcaFix}. Defaults to \code{1}.}
}
\description{
A step of the rangayyan_class2 implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{clean}, \code{components}, \code{kurtosis}, \code{artifacts}, \code{mixing}, \code{removedpower}, \code{method}.
}
\keyword{internal}
\examples{
set.seed(4)
S <- rbind(sin(2 * pi * (1:300) / 25), rnorm(300))
A <- matrix(c(1, 0.5, 0.4, 1), 2, 2)
X <- A \%*\% S
r <- IcaClean(X)
str(r, max.level = 1)
}
