% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{dual_objective}
\alias{dual_objective}
\title{dual_objective}
\usage{
dual_objective(alpha, y, K) 
}
\arguments{
\item{alpha}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{y}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{K}{A matrix; indexed by row and column.}
}
\description{
A step of the svmopt_native implementation. Called by \code{smo}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
set.seed(1)
X <- rbind(matrix(rnorm(10, -1), 5, 2), matrix(rnorm(10, 1), 5, 2))
y <- rep(c(-1, 1), each = 5)
K <- X \%*\% t(X)
dual_objective(rep(0.1, 10), y, K)
}
