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
\item{X}{A matrix; passed to \code{as.matrix}.}
\item{y}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{nu}{Numeric; combined arithmetically in the body. Defaults to \code{4}.}
\item{max_iter}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{200}.}
\item{tol}{Passed to \code{<}. Defaults to \code{1e-10}.}
\item{add_intercept}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
A step of the bayreg2_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{coefficients}, \code{std_error}, \code{weights}, \code{residuals}, \code{scale2}, \code{fitted}, \code{iterations}, \code{converged}, \code{nu}, \code{loglik}, \code{n}, \code{p}, \code{method}, \code{note}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_bayreg2_student_t_regression(V, V)
}
