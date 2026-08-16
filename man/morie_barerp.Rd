% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_barerp}
\alias{morie_barerp}
\title{morie_barerp}
\usage{
morie_barerp(f0, constraints, x0, t0 = 1, mu = 10, eps = 1e-08, aeq = NULL, beq = NULL, centering = "newton", tol = 1e-10, max_inner = 200L, max_outer = 200L, grad = NULL, hess = NULL, affine = FALSE)
}
\description{
morie_barerp
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
