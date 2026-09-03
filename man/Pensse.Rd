% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Pensse}
\alias{Pensse}
\title{Penalized sum of squared errors (MVSML eq. 14.10)}
\usage{
Pensse(y, X, beta, lam, P, mu = 0) 
}
\description{
SSE_lambda(beta) = sum_i ( y_i - mu - sum_l x_il beta_l )^2 + lambda J_beta, with J_beta the penalty (14.11).  lambda trades fit against smoothness: at lambda = 0 it is least squares, and as lambda grows beta(t) is driven towards a constant.
}
\arguments{
\item{y}{Argument \code{y}; see Usage.}
\item{X}{Argument \code{X}; see Usage.}
\item{beta}{Argument \code{beta}; see Usage.}
\item{lam}{Argument \code{lam}; see Usage.}
\item{P}{Argument \code{P}; see Usage.}
\item{mu}{Argument \code{mu}; see Usage.}
}
\value{
A list with \code{sse}, \code{penalty}, \code{lambda}, \code{objective}, \code{fitted}, \code{residuals}.
}
\keyword{internal}
\examples{
rmorie:::Pensse(y = c(1, 2, 3, 4, 5, 6, 7, 8), X = c(1, 2, 3, 4, 5, 6, 7, 8), beta = 0.5,
  lam = c(1, 2, 3, 4, 5, 6, 7, 8), P = 0.5)
}
