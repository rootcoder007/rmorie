% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{solve_pair}
\alias{solve_pair}
\title{solve_pair}
\usage{
solve_pair(i, j, alpha, y, K, grad, C) 
}
\arguments{
\item{i}{Passed to \code{.svmopt_bounds}.}
\item{j}{Passed to \code{.svmopt_bounds}.}
\item{alpha}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{y}{A vector; indexed elementwise.}
\item{K}{A matrix; indexed by row and column.}
\item{grad}{A vector; indexed elementwise.}
\item{C}{Passed to \code{.svmopt_bounds}.}
}
\description{
A step of the svmopt_native implementation. Called by \code{smo}. See the file header for the source the module follows.
}
\value{
A list with \code{alpha}, \code{moved}, \code{clipped}, \code{L}, \code{H}, \code{eta}, \code{step}.
}
\keyword{internal}
\examples{
set.seed(1)
X <- rbind(matrix(rnorm(10, -1), 5, 2), matrix(rnorm(10, 1), 5, 2))
y <- rep(c(-1, 1), each = 5)
K <- X \%*\% t(X)
a <- rep(0, 10)
grad <- rep(-1, 10)
r <- solve_pair(1L, 6L, a, y, K, grad, C = 1)
str(r, max.level = 1)
}
