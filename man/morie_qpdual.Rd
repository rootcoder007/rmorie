% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_qpdual}
\alias{morie_qpdual}
\title{Frank-Wolfe for a quadratic objective on a compact convex set}
\usage{
morie_qpdual(Q, c, x0 = NULL, domain = "simplex", lower = NULL, upper = NULL, step = "exact", max_iter = 1000L, tol = 1e-12)
}
\arguments{
\item{Q}{Symmetric n-by-n numeric matrix.}
\item{c}{Numeric vector of length \code{n}.}
\item{x0}{Optional starting point (default: simplex centre, box midpoint).}
\item{domain}{One of \code{"simplex"} or \code{"box"}.}
\item{lower,upper}{Optional numeric vectors; required for \code{box}.}
\item{step}{One of \code{"exact"} (line search on the quadratic)   or \code{"standard"} (the 2/(k+2) schedule).}
\item{max_iter}{Integer, maximum iterations.}
\item{tol}{Stop when the Frank-Wolfe gap drops below \code{tol}.}
}
\description{
Minimises \code{1/2 x\'Qx + c\'x} over the probability simplex or a box. Feasibility is preserved exactly: each iterate is a convex combination of the previous point and a vertex of \code{C}.
}
\value{
A list with \code{estimate}, \code{x}, \code{fun},   \code{gap}, \code{iterations}, \code{converged}, \code{domain},   \code{step}, \code{history}, \code{method}.
}
\keyword{internal}
