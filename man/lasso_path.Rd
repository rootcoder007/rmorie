% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{lasso_path}
\alias{lasso_path}
\title{Coordinate descent for the L1-penalised least squares fit}
\usage{
lasso_path(X, y, lam, iters = 500, tol = 1e-09)
}
\arguments{
\item{X}{Design matrix.}
\item{y}{Outcome vector.}
\item{lam}{Regularisation strength.}
\item{iters}{Maximum coordinate-descent passes.}
}
\description{
Coordinate descent for the L1-penalised least squares fit
}
\value{
A list with \code{beta}, \code{intercept}, \code{support},   \code{lambda}.
}
\keyword{internal}
