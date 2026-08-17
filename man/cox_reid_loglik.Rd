% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{cox_reid_loglik}
\alias{cox_reid_loglik}
\title{Cox-Reid adjusted log-likelihood (eq. 7)}
\usage{
cox_reid_loglik(alpha, K, mu, X)
}
\arguments{
\item{alpha}{Positive numeric.}
\item{K}{Count vector.}
\item{mu}{Fitted mean vector.}
\item{X}{Design matrix.}
}
\description{
\code{ell(alpha) - 0.5 log det(X\' W X)}. The adjustment is the GLM analogue of Bessel\'s correction.
}
\value{
Numeric scalar.
}
\keyword{internal}
\examples{
set.seed(1)
m <- 6
X <- cbind(1, rep(c(0, 1), each = 3))
mu <- rep(c(50, 80), each = 3)
K <- rpois(m, mu)
cox_reid_loglik(0.1, K, mu, X)
}
