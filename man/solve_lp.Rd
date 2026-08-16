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
\item{A}{See Usage.}
\item{b}{See Usage.}
\item{c}{See Usage.}
\item{tol}{Defaults to \code{1e-09}.}
\item{max_iter}{Defaults to \code{100L}.}
\item{nu}{Defaults to \code{3}.}
\item{eta}{Defaults to \code{0.9995}.}
\item{corrector}{Defaults to \code{TRUE}.}
}
\description{
Part of the mehtad_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{x}, \code{y}, \code{s}, \code{mu}, \code{objective}, \code{dual_objective}, \code{iterations}, \code{corrector}, \code{primal_residual}, \code{dual_residual}, \code{converged}, \code{method}, \code{note}.
}
\keyword{internal}
