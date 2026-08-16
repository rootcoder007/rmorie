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
