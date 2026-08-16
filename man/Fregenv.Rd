% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Fregenv}
\alias{Fregenv}
\title{Functional regression with environment effects (MVSML eqs. 14.13-14.14)}
\usage{
Fregenv(y, X, X_E, X_EF = NULL, lam = 0, P = NULL)
}
\description{
y = 1_n mu + X_E beta_E + X beta + e (14.13), and with the environment-by-reflectance interaction block, y = 1_n mu + X_E beta_E + X beta + X_EF beta_EF + e (14.14).  X carries the L1 functional scores of (14.4)-(14.5).  Passing X_EF = NULL gives (14.13) and passing it gives (14.14); the two differ by that block alone, which is why one routine covers both.
}
\value{
A list with \code{coef}, \code{mu}, \code{beta_E}, \code{beta}, \code{beta_EF}, \code{widths}, \code{design}, \code{fitted}, \code{residuals}, \code{sse}, \code{n_columns}, \code{has_interaction}.
}
\keyword{internal}
\examples{
Fregenv(y = c(1, 2, 3, 4, 5, 6, 7, 8), X = c(1, 2, 3, 4, 5, 6, 7, 8), X_E = c(1, 2, 3, 4, 5, 6, 7, 8))
}
