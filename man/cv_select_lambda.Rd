% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{cv_select_lambda}
\alias{cv_select_lambda}
\title{cv_select_lambda}
\usage{
cv_select_lambda(X, y, lambdas, V = 5L, seed = 0L, ...)
}
\arguments{
\item{X}{A matrix; passed to \code{as.matrix}.}
\item{y}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{lambdas}{See Usage.}
\item{V}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{5L}.}
\item{seed}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0L}.}
\item{...}{Passed through.}
}
\description{
A step of the tlhal_native implementation. Called by \code{morie_tlhal}. See the file header for the source the module follows.
}
\value{
A list with \code{lambda}, \code{cv_risks}, \code{note}.
}
\keyword{internal}
\examples{
set.seed(1)
X <- matrix(runif(60), 30, 2)
y <- sin(3 * X[, 1]) + X[, 2] + rnorm(30, 0, 0.1)
r <- cv_select_lambda(X, y, lambdas = c(0.5, 1, 2), V = 3L, iters = 150L)
str(r, max.level = 1)
}
