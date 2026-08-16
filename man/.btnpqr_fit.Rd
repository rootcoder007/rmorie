% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.btnpqr_fit}
\alias{.btnpqr_fit}
\title{.btnpqr_fit}
\usage{
.btnpqr_fit(Xm, yy, tau, maxit = 200L, tol = 1e-10, eps = 1e-06)
}
\description{
.btnpqr_fit
}
\value{
The value of \code{b}, as built in the body.
}
\keyword{internal}
\examples{
X <- cbind(1, 1:20); y <- 1 + 0.4 * (1:20) + c(rep(0, 10), rep(1, 10))
Btnpqr(X, y, 0.5, B = 20)$beta_hat
.btnpqr_fit(Xm = X, yy = y, tau = X)
}
