% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_tmle_ols_predict}
\alias{.morie_tmle_ols_predict}
\title{OLS fitted on `fit_rows` only, predicted for everyone}
\usage{
.morie_tmle_ols_predict(X, y, fit_rows)
}
\arguments{
\item{X}{Passed to \code{cbind}.}
\item{y}{A vector; indexed elementwise.}
\item{fit_rows}{See Usage.}
}
\description{
A step of the tmle_native implementation. Called by \code{morie_tmle_ate}. See the file header for the source the module follows.
}
\value{
The value of \code{as.vector}.
}
\keyword{internal}
\examples{
.morie_tmle_ols_predict(X = c(1, 2, 3, 4, 5, 6, 7, 8), y = c(1, 2, 3, 4, 5, 6, 7, 8), fit_rows = c(1, 2, 3, 4, 5, 6, 7, 8))
}
