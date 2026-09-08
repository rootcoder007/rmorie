% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_geron_random_subspaces}
\alias{morie_geron_random_subspaces}
\title{Random subspaces: feature bagging without row subsampling (Geron Ch 6, hmrsp)}
\usage{
morie_geron_random_subspaces(X, y, base_estimator = NULL, n_estimators = 10, max_features = NULL, 
    seed = 0, task = "auto") 
}
\arguments{
\item{X,y}{Data and targets.}
\item{base_estimator}{Optional `base_estimator(X_sub, y) -> predict`.}
\item{n_estimators,max_features,seed,task}{Controls.}
}
\description{
Random subspaces: feature bagging without row subsampling (Geron Ch 6, hmrsp)
}
\value{
A list with \code{predict}, \code{train_pred}, \code{train_mse}, \code{feature_sets}, \code{feature_usage}, \code{estimators}, \code{max_features}, \code{task}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_geron_random_subspaces(V, V)
}
