% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_cv_score}
\alias{.schab_cv_score}
\title{Spgwr::gwr.cv.f -- leave-one-out; y_i never predicts itself}
\usage{
.schab_cv_score(y, X, distances, bandwidth, kernel = "gaussian", adaptive = FALSE)
}
\arguments{
\item{y}{A matrix; passed to \code{\%*\%}.}
\item{X}{A matrix; indexed by row and column.}
\item{distances}{A matrix; passed to \code{as.matrix}.}
\item{bandwidth}{Passed to \code{.schab_local_weights}.}
\item{kernel}{Passed to \code{.schab_local_weights}. Defaults to \code{"gaussian"}.}
\item{adaptive}{Passed to \code{.schab_local_weights}. Defaults to \code{FALSE}.}
}
\description{
A step of the schab_gwr_shared implementation. Called by \code{.schab_gwr_criterion}, \code{spgwrb}. See the file header for the source the module follows.
}
\value{
The value of \code{total}, as built in the body.
}
\keyword{internal}
\examples{
.schab_cv_score(y = c(1, 2, 3, 4, 5, 6, 7, 8), X = c(1, 2, 3, 4, 5, 6, 7, 8), distances = c(1, 2, 3, 4, 5, 6, 7, 8), bandwidth = 0.5)
}
