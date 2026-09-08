% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{barrier_lp}
\alias{barrier_lp}
\title{Barrier method on a linear program}
\usage{
barrier_lp(c, A_ub, b_ub, A_eq = NULL, b_eq = NULL, x0 = NULL, ...) 
}
\arguments{
\item{c}{Cost vector.}
\item{A_ub}{Inequality matrix.}
\item{b_ub}{Inequality right-hand side.}
\item{A_eq}{Optional equality matrix.}
\item{b_eq}{Optional equality right-hand side.}
\item{x0}{Optional strictly feasible starting point.}
\item{...}{Forwarded to \code{barrier_method}.}
}
\description{
Minimise \eqn{c^T x} subject to \eqn{A_ub x <= b_ub} and optionally \eqn{A_eq x = b_eq}. With no \code{x0} a strictly feasible point is found by \code{phase1} first.
}
\value{
A list, see \code{barrier_method}.
}
\keyword{internal}
\examples{
r <- barrier_lp(c = c(1, 1), A_ub = rbind(c(-1, 0), c(0, -1)),
                b_ub = c(-1, -1))
r$x
}
