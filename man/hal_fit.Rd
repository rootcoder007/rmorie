% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{hal_fit}
\alias{hal_fit}
\title{hal_fit}
\usage{
hal_fit(X, y, lam = 1, iters = 2000L, step = 0.05, max_order = 2L, knots = NULL, intercept = TRUE)
}
\arguments{
\item{X}{Passed to \code{indicator_basis}.}
\item{y}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{lam}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.}
\item{iters}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{2000L}.}
\item{step}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.05}.}
\item{max_order}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{2L}.}
\item{knots}{Passed to \code{indicator_basis}.}
\item{intercept}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
A step of the tlhal_native implementation. Called by \code{cv_select_lambda}, \code{morie_tlhal}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{beta}, \code{intercept}, \code{columns}, \code{n_basis}, \code{variation_norm}, \code{lambda}, \code{mse}, \code{mse_history}, \code{max_order}, \code{method}, \code{note}.
}
\keyword{internal}
\examples{
set.seed(1)
X <- matrix(runif(60), 30, 2)
y <- sin(3 * X[, 1]) + X[, 2] + rnorm(30, 0, 0.1)
r <- hal_fit(X, y, lam = 1, iters = 300L)
str(r, max.level = 1)
}
