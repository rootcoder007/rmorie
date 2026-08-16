% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_gr_mse_core}
\alias{.morie_gr_mse_core}
\title{Grmse core, shared by every regularised cost and every GD driver here}
\usage{
.morie_gr_mse_core(X, y, theta)
}
\arguments{
\item{X}{See Usage.}
\item{y}{See Usage.}
\item{theta}{See Usage.}
}
\description{
Part of the geron_train_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{cost}, \code{rmse}, \code{residuals}, \code{predictions}, \code{n}.
}
\keyword{internal}
