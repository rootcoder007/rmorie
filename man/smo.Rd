% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{smo}
\alias{smo}
\title{smo}
\usage{
smo(y, K, C = 1, tol = 1e-08, max_iter = 20000)
}
\arguments{
\item{y}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{K}{A matrix; indexed by row and column.}
\item{C}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.}
\item{tol}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1e-08}.}
\item{max_iter}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{20000}.}
}
\description{
A step of the svmopt_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{alpha}, \code{b}, \code{gap}, \code{iterations}, \code{converged}, \code{support_vectors}, \code{n_sv}, \code{n_free}, \code{equality_residual}, \code{objective}, \code{method}, \code{note}.
}
\keyword{internal}
\examples{
set.seed(1)
X <- rbind(matrix(rnorm(20, -1.5), 10, 2), matrix(rnorm(20, 1.5), 10, 2))
y <- rep(c(-1, 1), each = 10)
K <- X \%*\% t(X)
r <- smo(y, K, C = 1)
str(r, max.level = 1)
}
