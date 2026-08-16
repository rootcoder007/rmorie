% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{phase1}
\alias{phase1}
\title{Phase I}
\usage{
phase1(cons, x0, aeq = NULL, beq = NULL, max_outer = 60L, ...)
}
\arguments{
\item{cons}{List of constraints.}
\item{x0}{Starting point.}
\item{aeq}{Optional equality-constraint matrix.}
\item{beq}{Optional equality right-hand side.}
\item{max_outer}{Maximum outer iterations for the inner barrier   method.}
\item{...}{Forwarded to \code{barrier_method}.}
}
\description{
Boyd section 11.4: minimise \eqn{s} subject to \eqn{f_i(x) <= s}. The original problem is strictly feasible exactly when the optimal \eqn{s} is negative.
}
\value{
List with \code{x}, \code{s}, \code{feasible}, \code{outer},   \code{newton}.
}
\keyword{internal}
\examples{
cons <- list(function(x) 1 - x[1], function(x) x[1] - 5)
r <- phase1(cons, x0 = c(0.5, 0))
str(r, max.level = 1)
}
