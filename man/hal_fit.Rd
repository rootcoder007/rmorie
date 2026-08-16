% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{hal_fit}
\alias{hal_fit}
\title{hal_fit}
\usage{
hal_fit(X, y, lam = 1, iters = 2000L, step = 0.05, max_order = 2L, knots = NULL, intercept = TRUE)
}
\arguments{
\item{X}{See Usage.}
\item{y}{See Usage.}
\item{lam}{Defaults to \code{1}.}
\item{iters}{Defaults to \code{2000L}.}
\item{step}{Defaults to \code{0.05}.}
\item{max_order}{Defaults to \code{2L}.}
\item{knots}{Defaults to \code{NULL}.}
\item{intercept}{Defaults to \code{TRUE}.}
}
\description{
Part of the tlhal_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{beta}, \code{intercept}, \code{columns}, \code{n_basis}, \code{variation_norm}, \code{lambda}, \code{mse}, \code{mse_history}, \code{max_order}, \code{method}, \code{note}.
}
\keyword{internal}
