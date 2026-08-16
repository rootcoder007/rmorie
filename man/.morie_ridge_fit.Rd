% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_ridge_fit}
\alias{.morie_ridge_fit}
\title{Ridge coefficients with an unpenalised intercept}
\usage{
.morie_ridge_fit(X, y, lam = 0.001)
}
\arguments{
\item{X}{See Usage.}
\item{y}{A matrix; passed to \code{crossprod}.}
\item{lam}{Numeric; combined arithmetically in the body. Defaults to \code{0.001}.}
}
\description{
A step of the causal_shared_native implementation. Called by \code{morie_dr_learner}. See the file header for the source the module follows.
}
\value{
A matrix, from \code{solve}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_ridge_fit(V, V)
}
