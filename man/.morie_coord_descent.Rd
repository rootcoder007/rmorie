% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_coord_descent}
\alias{.morie_coord_descent}
\title{.morie_coord_descent}
\usage{
.morie_coord_descent(X, y, alpha, lambda, max_iter = 1000L, tol = 1e-06, warm = NULL)
}
\arguments{
\item{X}{See Usage.}
\item{y}{See Usage.}
\item{alpha}{See Usage.}
\item{lambda}{See Usage.}
\item{max_iter}{Defaults to \code{1000L}.}
\item{tol}{Defaults to \code{1e-06}.}
\item{warm}{Defaults to \code{NULL}.}
}
\description{
Elastic-net coordinate descent on standardized covariates -- the shared core behind morie_penalized_regression() and morie_regularization_path() (replacing their glmnet delegation). Returns coefficients on the ORIGINAL scale plus intercept. `warm` optionally seeds beta (standardized scale) for path fits.
}
\value{
A list with \code{beta}, \code{beta_std}, \code{intercept}, \code{n_iter}.
}
\keyword{internal}
