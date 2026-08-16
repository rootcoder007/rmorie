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
\item{X}{A matrix; passed to \code{nrow}.}
\item{y}{A vector; its length is taken.}
\item{theta}{A matrix; passed to \code{\%*\%}.}
}
\description{
A step of the geron_train_native implementation. Called by \code{morie_geron_ch4_mse_gradient_vector}, \code{morie_geron_ch4_normal_equation}, \code{morie_geron_early_stopping} and 4 others in the module. See the file header for the source the module follows.
}
\value{
A list with \code{cost}, \code{rmse}, \code{residuals}, \code{predictions}, \code{n}.
}
\keyword{internal}
\examples{
.morie_gr_mse_core(X = 5L, y = 5L, theta = 0.5)
}
