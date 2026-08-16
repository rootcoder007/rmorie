% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_ridge_fit}
\alias{.morie_ridge_fit}
\title{Ridge coefficients with an unpenalised intercept}
\usage{
.morie_ridge_fit(X, y, lam = 0.001)
}
\arguments{
\item{X}{See Usage.}
\item{y}{See Usage.}
\item{lam}{Defaults to \code{0.001}.}
}
\description{
Part of the causal_shared_native implementation; see the file header for the source it follows.
}
\value{
A matrix, from \code{solve}.
}
\keyword{internal}
