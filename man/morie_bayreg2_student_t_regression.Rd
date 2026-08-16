% Generated from the module source by ledger/gen_rd.R -- roxygen2 hangs
% on this package. Usage comes from args(), so it tracks the formals.
\name{morie_bayreg2_student_t_regression}
\alias{morie_bayreg2_student_t_regression}
\alias{morie_bayreg2}
\title{Student-t linear model by EM as a scale mixture of}
\usage{
morie_bayreg2_student_t_regression(X, y, nu = 4, max_iter = 200, tol = 1e-10, add_intercept = TRUE)
}
\description{
R arm of bayreg2 -- Student-t linear model by EM as a scale mixture of 881-896. Mirrors src/morie/fn/bayreg2.py.
}
\value{
A list with components \code{n}, \code{beta}, \code{A}, \code{estimate}, \code{coefficients}, \code{std_error}, \code{weights}, \code{residuals}, \code{scale2}, \code{fitted}, \code{iterations}, \code{converged}, \code{nu}, \code{loglik}, \code{p}, \code{method}, \code{note}.
}
\references{
normals. West, M. (1984) JRSS B 46(3), 431-439; Geweke, J. (1993) J. Appl.
Econometrics 8(S1), S19-S40; Lange, Little & Taylor (1989) JASA 84(408),
}
\keyword{internal}
