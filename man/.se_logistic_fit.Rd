% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.se_logistic_fit}
\alias{.se_logistic_fit}
\title{.se_logistic_fit}
\usage{
.se_logistic_fit(X, y, max_iter = 200L, tol = 1e-06)
}
\description{
.se_logistic_fit
}
\value{
The value of \code{beta}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.se_logistic_fit(V, V)
}
