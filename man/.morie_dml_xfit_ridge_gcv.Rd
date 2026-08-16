% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_dml_xfit_ridge_gcv}
\alias{.morie_dml_xfit_ridge_gcv}
\title{Internal helper: cross-fit ridge with per-fold GCV lambda}
\usage{
.morie_dml_xfit_ridge_gcv(X, y, n_folds = 5L, random_state = 42L)
}
\description{
Internal helper: cross-fit ridge with per-fold GCV lambda
}
\value{
The value of \code{pred}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
.morie_dml_xfit_ridge_gcv(M, V)
}
