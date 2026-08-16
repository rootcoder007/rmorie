% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_geron_regression_tree}
\alias{morie_geron_regression_tree}
\title{CART regression tree minimising per-leaf MSE (Geron Ch 5, hmrdt)}
\usage{
morie_geron_regression_tree(X, y, max_depth = 3, min_samples_leaf = 1)
}
\arguments{
\item{X,y}{Data and targets.}
\item{max_depth,min_samples_leaf}{Regularisation.}
}
\description{
CART regression tree minimising per-leaf MSE (Geron Ch 5, hmrdt)
}
\value{
A list with \code{tree}, \code{predict}, \code{predictions}, \code{mse}, \code{n_leaves}, \code{depth}, \code{feature_importance}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_geron_regression_tree(V, V)
}
