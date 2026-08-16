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
\item{x_train}{A matrix; passed to \code{nrow}.}
\item{z_train}{Numeric; passed to \code{mean}.}
\item{x_test}{A matrix; passed to \code{as.matrix}.}
\item{n_folds}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{5L}.}
\item{lambdas}{Optional; may be \code{NULL}. A vector; its length is taken and its elements indexed.}
}
\description{
Ridge regression with SVD path + k-fold CV lambda selection, then prediction -- the cv.glmnet(alpha = 0, s = "lambda.min") surface the DML cross-fit nuisance learners used. The SVD makes the whole lambda path essentially free.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
.morie_cv_ridge_predict(x_train = c(1, 2, 3, 4, 5, 6, 7, 8), z_train = c(1, 2, 3, 4, 5, 6, 7, 8), x_test = c(1, 2, 3, 4, 5, 6, 7, 8))
}
