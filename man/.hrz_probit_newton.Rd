% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.hrz_probit_newton}
\alias{.hrz_probit_newton}
\title{Internal helper: Hrz Probit Newton}
\usage{
.hrz_probit_newton(D, Z, maxit = 50, tol = 1e-08)
}
\description{
Internal helper: Hrz Probit Newton
}
\value{
A vector.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
.hrz_probit_newton(M, M)
}
