% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{barrier_method}
\alias{barrier_method}
\title{Barrier method}
\usage{
barrier_method(f0, constraints, x0, t0 = 1, mu = 10, eps = 1e-08, aeq = NULL, 
    beq = NULL, centering = "newton", tol = 1e-10, max_inner = 200L, max_outer = 200L, 
    grad = NULL, hess = NULL, affine = FALSE) 
}
\arguments{
\item{f0}{Objective: a function, a \code{Fun}, or a list with   \code{f}, \code{grad}, \code{hess}, \code{affine}.}
\item{constraints}{List of inequality constraints in the same   form.}
\item{x0}{Strictly feasible starting point; use \code{phase1} to   find one.}
\item{t0}{Initial centering parameter, positive.}
\item{mu}{Growth factor, > 1.}
\item{eps}{Duality-gap tolerance, positive.}
\item{aeq}{Optional equality-constraint matrix.}
\item{beq}{Optional equality right-hand side.}
\item{centering}{\code{"newton"} (default), \code{"gradient"}, or   \code{"none"}.}
\item{tol}{Newton (or gradient) decrement tolerance.}
\item{max_inner}{Maximum centering iterations.}
\item{max_outer}{Maximum outer iterations.}
\item{grad}{Optional gradient override (forwarded to \code{_as_fun}   when \code{f0} is a function).}
\item{hess}{Optional Hessian override.}
\item{affine}{Optional affine flag override.}
}
\description{
Boyd Algorithm 11.1, the logarithmic barrier method of Frisch (1956) equation 5.1. The central point carries its own certificate (Boyd eq. 11.10-11.12), so the returned \code{gap = m/t} is an exact duality gap, not an estimate.
}
\value{
A named list mirroring the Python RichResult payload:   \code{x}, \code{fun}, \code{gap}, \code{t}, \code{lambda_},   \code{slack}, \code{outer}, \code{newton}, \code{decrement},   \code{history}, \code{centering}, \code{converged}, plus   \code{steps_predicted}, \code{method}, \code{note} on the   multi-step path.
}
\keyword{internal}
\examples{
f0 <- function(x) sum(x^2)
cons <- list(function(x) 1 - x[1])
r <- barrier_method(f0, cons, x0 = c(2, 0))
r$x
}
