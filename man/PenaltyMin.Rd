% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{PenaltyMin}
\alias{PenaltyMin}
\title{\\' Minimise f subject to g_i(x) <= 0 by Courant\\'s quadratic}
\usage{
PenaltyMin(f, constraints, x0, mu, n_outer = 8L, growth = 10, n_inner = 200L, step0 = 1, h = 1e-06, armijo = 1e-04, max_halving = 40L)
}
\arguments{
\item{f}{Passed to \code{.morie_t2_qpen}.}
\item{constraints}{Passed to \code{.morie_t2_qpen}.}
\item{x0}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{mu}{Numeric; combined arithmetically in the body.}
\item{n_outer}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{8L}.}
\item{growth}{Numeric; combined arithmetically in the body. Defaults to \code{10}.}
\item{n_inner}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{200L}.}
\item{step0}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.}
\item{h}{Passed to \code{.morie_t2_fdgrad}. Defaults to \code{1e-06}.}
\item{armijo}{Numeric; combined arithmetically in the body. Defaults to \code{1e-04}.}
\item{max_halving}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{40L}.}
}
\description{
penalty \\' \\' Q(x; mu) = f(x) + mu sum_i max(0, g_i(x))^2, minimised for an \\' increasing sequence of mu.  The inner solve is steepest descent with \\' an Armijo backtracking line search on central finite differences, run \\' for a fixed budget. \\' \\' @param f objective, taking a numeric vector \\' @param constraints list of functions; g_i(x) <= 0 is the constraint \\' @param x0 starting point \\' @param mu initial penalty weight \\' @param n_outer,growth,n_inner,step0,h,armijo,max_halving fixed budget \\'
}
\value{
A list with \code{x}, \code{f}, \code{penalty}, \code{violation}, \code{max_violation}, \code{q}, \code{mu}, \code{n_outer}, \code{n_inner}, \code{method}.
}
\keyword{internal}
