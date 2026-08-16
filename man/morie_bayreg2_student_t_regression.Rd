% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_bayreg2_student_t_regression}
\alias{morie_bayreg2_student_t_regression}
\title{morie_bayreg2_student_t_regression}
\usage{
morie_bayreg2_student_t_regression(X, y, nu = 4, max_iter = 200, tol = 1e-10, add_intercept = TRUE)
}
\arguments{
\item{X}{See Usage.}
\item{y}{See Usage.}
\item{nu}{Defaults to \code{4}.}
\item{max_iter}{Defaults to \code{200}.}
\item{tol}{Defaults to \code{1e-10}.}
\item{add_intercept}{Defaults to \code{TRUE}.}
}
\description{
Part of the bayreg2_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{coefficients}, \code{std_error}, \code{weights}, \code{residuals}, \code{scale2}, \code{fitted}, \code{iterations}, \code{converged}, \code{nu}, \code{loglik}, \code{n}, \code{p}, \code{method}, \code{note}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_bayreg2_student_t_regression(V, V)
}
