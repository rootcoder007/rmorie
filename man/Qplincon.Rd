% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Qplincon}
\alias{Qplincon}
\title{Quadratic program under one linear inequality (MVSML eqs. 9.15-9.26)}
\usage{
Qplincon(a, c) 
}
\description{
minimize z\'z subject to a\'z >= c.  The Wolfe dual (9.17) is L = z\'z - 2 alpha (a\'z - c); stationarity (9.18) gives z = alpha a, and substituting back leaves L(alpha) = -(a\'a) alpha^2 + 2 c alpha (9.19), maximized at alpha = c / (a\'a) >= 0 (9.20).  Illustrative Example 9.1 is a = 1, c = 1; Illustrative Example 9.2 is a = (1, 1), c = 2.  The two are the same problem, so one routine answers both.
}
\arguments{
\item{a}{Argument \code{a}; see Usage.}
\item{c}{Argument \code{c}; see Usage.}
}
\value{
A list with \code{x}, \code{alpha}, \code{dual_quadratic}, \code{dual_linear}, \code{dual_value}, \code{primal_value}, \code{constraint}, \code{active}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
rmorie:::Qplincon(V, V)
}
