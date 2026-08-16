% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{solve_lp}
\alias{solve_lp}
\title{solve_lp}
\usage{
solve_lp(A, b, c, tol = 1e-09, max_iter = 100L, nu = 3, eta = 0.9995, corrector = TRUE)
}
\arguments{
\item{A}{A matrix; passed to \code{as.matrix}.}
\item{b}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{c}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{tol}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1e-09}.}
\item{max_iter}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{100L}.}
\item{nu}{Passed to \code{centering_parameter}. Defaults to \code{3}.}
\item{eta}{Passed to \code{max_step}. Defaults to \code{0.9995}.}
\item{corrector}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
A step of the mehtad_native implementation. Called by \code{morie_mehtad}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{x}, \code{y}, \code{s}, \code{mu}, \code{objective}, \code{dual_objective}, \code{iterations}, \code{corrector}, \code{primal_residual}, \code{dual_residual}, \code{converged}, \code{method}, \code{note}.
}
\keyword{internal}
