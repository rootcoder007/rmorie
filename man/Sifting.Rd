% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Sifting}
\alias{Sifting}
\title{Eq (3.28): integral_{T1}^{T2} x(t) delta(t - to) dt = x(to) if}
\usage{
Sifting(x, t0, lower, upper)
}
\arguments{
\item{x}{See Usage.}
\item{t0}{See Usage.}
\item{lower}{See Usage.}
\item{upper}{See Usage.}
}
\description{
T1 < to < T2, else 0.  Both inequalities are strict, so an impulse sitting on a limit contributes nothing.
}
\value{
A list with \code{value}, \code{inside}, \code{t0}, \code{lower}, \code{upper}, \code{method}.
}
\keyword{internal}
