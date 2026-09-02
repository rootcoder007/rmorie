% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Sifting}
\alias{Sifting}
\title{Eq (3.28): integral_\{T1\}^\{T2\} x(t) delta(t - to) dt = x(to) if}
\usage{
Sifting(x, t0, lower, upper) 
}
\arguments{
\item{x}{A function; the body checks with \code{is.function}.}
\item{t0}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{lower}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{upper}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
T1 < to < T2, else 0.  Both inequalities are strict, so an impulse sitting on a limit contributes nothing.
}
\value{
A list with \code{value}, \code{inside}, \code{t0}, \code{lower}, \code{upper}, \code{method}.
}
\keyword{internal}
\examples{
Sifting(function(t) t^2 + 1, 2, 0, 5)
}
