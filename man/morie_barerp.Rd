% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_barerp}
\alias{morie_barerp}
\title{morie_barerp}
\usage{
morie_barerp(f0, constraints, x0, t0 = 1, mu = 10, eps = 1e-08, aeq = NULL, 
    beq = NULL, centering = "newton", tol = 1e-10, max_inner = 200L, max_outer = 200L, 
    grad = NULL, hess = NULL, affine = FALSE) 
}
\description{
morie_barerp
}
\arguments{
\item{f0}{Argument \code{f0}; see Usage.}
\item{constraints}{Argument \code{constraints}; see Usage.}
\item{x0}{Argument \code{x0}; see Usage.}
\item{t0}{Argument \code{t0}; see Usage.}
\item{mu}{Argument \code{mu}; see Usage.}
\item{eps}{Argument \code{eps}; see Usage.}
\item{aeq}{Argument \code{aeq}; see Usage.}
\item{beq}{Argument \code{beq}; see Usage.}
\item{centering}{Argument \code{centering}; see Usage.}
\item{tol}{Argument \code{tol}; see Usage.}
\item{max_inner}{Argument \code{max_inner}; see Usage.}
\item{max_outer}{Argument \code{max_outer}; see Usage.}
\item{grad}{Argument \code{grad}; see Usage.}
\item{hess}{Argument \code{hess}; see Usage.}
\item{affine}{Argument \code{affine}; see Usage.}
}
\value{
The value of \code{barrier_method}.
}
\keyword{internal}
\examples{
f0 <- function(x) sum(x^2)
cons <- list(function(x) 1 - x[1])
r <- morie_barerp(f0, cons, x0 = c(2, 0))
r$x
}
