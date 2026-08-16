% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_geron_random_patches}
\alias{morie_geron_random_patches}
\title{Random patches: subsample BOTH rows and features per base model (Geron Ch 6, hmrpt)}
\usage{
morie_geron_random_patches(X, y, base_estimator = NULL, n_estimators = 10, max_samples = NULL, max_features = NULL, seed = 0, task = "auto", bootstrap = FALSE)
}
\arguments{
\item{X,y}{Data and targets.}
\item{base_estimator}{Optional `base_estimator(Xp, yp) -> predict`.}
\item{n_estimators,max_samples,max_features,seed,task,bootstrap}{Controls.}
}
\description{
Random patches: subsample BOTH rows and features per base model (Geron Ch 6, hmrpt)
}
\value{
A list with \code{predict}, \code{train_pred}, \code{train_mse}, \code{patches}, \code{feature_usage}, \code{row_usage}, \code{estimators}, \code{max_samples}, \code{max_features}, \code{task}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
