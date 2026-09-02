% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Svmsdual}
\alias{Svmsdual}
\title{Wolfe dual of the support vector classifier (MVSML eqs. 9.44-9.45)}
\usage{
Svmsdual(X, y, T, K = NULL, ...) 
}
\description{
maximize L(alpha) = sum_i alpha_i - (1/2) sum_i sum_j alpha_i alpha_j y_i y_j (x_i . x_j) subject to 0 <= alpha_i <= T and sum_i alpha_i y_i = 0.  It differs from the hard margin dual (9.32)-(9.33) only by the upper bound T.
}
\arguments{
\item{X}{Argument \code{X}; see Usage.}
\item{y}{Argument \code{y}; see Usage.}
\item{T}{Argument \code{T}; see Usage.}
\item{K}{Argument \code{K}; see Usage.}
}
\value{
A list with \code{alpha}, \code{beta}, \code{beta0}, \code{objective}, \code{support_vectors}, \code{balance}, \code{bounded}, \code{at_bound}.
}
\keyword{internal}
\examples{
rmorie:::Svmsdual(X = c(1, 2, 3, 4, 5, 6, 7, 8), y = c(1, 2, 3, 4, 5, 6, 7, 8), T = c(1, 2, 3, 4, 5, 6, 7, 8))
}
