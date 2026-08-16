% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{DictCode}
\alias{DictCode}
\title{Section 9.5 for the greedy stage and 9.8 for the use of the resulting}
\usage{
DictCode(Y, D, sparsity, tol = 1e-12)
}
\arguments{
\item{Y}{See Usage.}
\item{D}{See Usage.}
\item{sparsity}{See Usage.}
\item{tol}{Defaults to \code{1e-12}.}
}
\description{
coefficients and reconstruction error as features; the least-squares reprojection is Pati, Rezaiifar and Krishnaprasad, Asilomar 1993, which Rangayyan does not cover.  Learning a dictionary and using one are separate steps: here the coefficients, not the signals, are the feature vectors.
}
\value{
A list with \code{coefficients}, \code{support}, \code{reconstruction}, \code{residual}, \code{error}, \code{method}.
}
\keyword{internal}
\examples{
DictCode(Y = c(1, 2, 3, 4, 5, 6, 7, 8), D = c(1, 2, 3, 4, 5, 6, 7, 8), sparsity = 5L)
}
