% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_cv_ridge_predict}
\alias{.morie_cv_ridge_predict}
\title{.morie_cv_ridge_predict}
\usage{
.morie_cv_ridge_predict(x_train, z_train, x_test, n_folds = 5L, lambdas = NULL)
}
\arguments{
\item{x_train}{See Usage.}
\item{z_train}{See Usage.}
\item{x_test}{See Usage.}
\item{n_folds}{Defaults to \code{5L}.}
\item{lambdas}{Defaults to \code{NULL}.}
}
\description{
Ridge regression with SVD path + k-fold CV lambda selection, then prediction -- the cv.glmnet(alpha = 0, s = "lambda.min") surface the DML cross-fit nuisance learners used. The SVD makes the whole lambda path essentially free.
}
\value{
A numeric value.
}
\keyword{internal}
