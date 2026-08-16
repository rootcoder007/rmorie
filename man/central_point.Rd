% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{central_point}
\alias{central_point}
\title{Central point}
\usage{
central_point(f0, cons, x, t, aeq = NULL, centering = "newton", tol = 1e-10, max_iter = 200L, alpha = 0.01, beta = 0.5, step0 = 1)
}
\arguments{
\item{f0}{Objective as a \code{Fun}, a function, or a list.}
\item{cons}{List of constraints in the same form.}
\item{x}{Starting point.}
\item{t}{Centering parameter.}
\item{aeq}{Optional equality-constraint matrix.}
\item{centering}{\code{"newton"} or \code{"gradient"}.}
\item{tol}{Tolerance on the decrement.}
\item{max_iter}{Maximum Newton (or gradient) iterations.}
\item{alpha}{Backtracking line-search parameter.}
\item{beta}{Backtracking line-search shrink factor.}
\item{step0}{Initial line-search step.}
}
\description{
Compute \eqn{x*(t) = argmin t f0 + phi}. \code{centering="newton"} uses Newton\'s method with backtracking line search and stops on the Newton decrement; \code{"gradient"} takes Frisch\'s compromise as a plain descent step on the same function.
}
\value{
List with \code{x}, \code{iters}, \code{decrement}.
}
\keyword{internal}
\examples{
f0 <- function(x) sum(x^2)
cons <- list(function(x) 1 - x[1], function(x) x[1] - 5)
r <- central_point(f0, cons, x = c(2, 0), t = 10)
str(r, max.level = 1)
}
