% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{kkt_violation}
\alias{kkt_violation}
\title{kkt_violation}
\usage{
kkt_violation(alpha, y, grad, C) 
}
\arguments{
\item{alpha}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{y}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{grad}{A vector; indexed elementwise.}
\item{C}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the svmopt_native implementation. Called by \code{recover_bias}, \code{smo}. See the file header for the source the module follows.
}
\value{
A list with \code{gap}, \code{i}, \code{j}, \code{n_up}, \code{n_low}.
}
\keyword{internal}
\examples{
set.seed(1)
X <- rbind(matrix(rnorm(10, -1), 5, 2), matrix(rnorm(10, 1), 5, 2))
y <- rep(c(-1, 1), each = 5)
K <- X \%*\% t(X)
a <- rep(0.1, 10)
grad <- as.numeric(K \%*\% (a * y) * y) - 1
str(kkt_violation(a, y, grad, C = 1), max.level = 1)
}
