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
\item{X}{A matrix; passed to \code{nrow}.}
\item{y}{Numeric; passed to \code{mean}.}
\item{alpha}{Passed to \code{.morie_coord_descent_cpp}.}
\item{lambda}{Passed to \code{.morie_coord_descent_cpp}.}
\item{max_iter}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{1000L}.}
\item{tol}{Passed to \code{.morie_coord_descent_cpp}. Defaults to \code{1e-06}.}
\item{warm}{Optional; may be \code{NULL}. Passed to \code{is.null}.}
}
\description{
Elastic-net coordinate descent on standardized covariates -- the shared core behind morie_penalized_regression() and morie_regularization_path() (replacing their glmnet delegation). Returns coefficients on the ORIGINAL scale plus intercept. `warm` optionally seeds beta (standardized scale) for path fits.
}
\value{
A list with \code{beta}, \code{beta_std}, \code{intercept}, \code{n_iter}.
}
\keyword{internal}
