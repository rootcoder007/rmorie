% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.dml_xfit_ridge}
\alias{.dml_xfit_ridge}
\title{Internal helper: Dml Xfit Ridge}
\usage{
.dml_xfit_ridge(X, y, n_folds = 5L, lambda = 1, random_state = 42L)
}
\description{
Internal helper: Dml Xfit Ridge
}
\value{
The value of \code{pred}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
.dml_xfit_ridge(M, V)
}
