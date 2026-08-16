% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{SimplexLP}
\alias{SimplexLP}
\title{\\\' Maximise c\\\'x subject to A x <= b, x >= 0, with b >= 0}
\usage{
SimplexLP(c, A, b, max_iter = 1000L, tol = 1e-12)
}
\arguments{
\item{c}{See Usage.}
\item{A}{See Usage.}
\item{b}{See Usage.}
\item{max_iter}{Defaults to \code{1000L}.}
\item{tol}{Defaults to \code{1e-12}.}
}
\description{
\\\' \\\' Tableau simplex with Bland\\\'s rule: the entering column is the \\\' lowest-indexed one with a negative reduced cost in the objective row, \\\' and among rows attaining the minimum ratio the leaving row is the one \\\' whose basic variable has the smallest index.  Bland\\\'s rule cannot \\\' cycle, and being a pure index rule it makes the R and Python arms \\\' pivot identically.  Requires b >= 0 so the slack basis is feasible; \\\' a negative right-hand side needs a phase-1 problem, not built here. \\\' Checked against lpSolve::lp. \\\' \\\' @param c objective coefficients; the objective is MAXIMISED \\\' @param A constraint matrix, ncon x nvar \\\' @param b right-hand side, every entry non-negative \\\' @param max_iter pivot budget \\\' @param tol zero tolerance for reduced costs and pivot elements \\\' @return list(status, x, objective, slack, basis, dual, iterations, \\\' n_var, n_con, method) \\\' @export
}
\value{
A list with \code{status}, \code{x}, \code{objective}, \code{slack}, \code{basis}, \code{dual}, \code{iterations}, \code{n_var}, \code{n_con}, \code{method}.
}
\keyword{internal}
\examples{
SimplexLP(c = c(1, 2, 3, 4, 5, 6, 7, 8), A = c(1, 2, 3, 4, 5, 6, 7, 8), b = 5L)
}
