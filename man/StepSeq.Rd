% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{StepSeq}
\alias{StepSeq}
\title{Eq (3.35): u(n) = 1 for n >= 0, 0 otherwise.  Non-strict, so}
\usage{
StepSeq(n, shift = 0)
}
\arguments{
\item{n}{See Usage.}
\item{shift}{Defaults to \code{0}.}
}
\description{
u(0) = 1 -- the opposite of eq (3.27).
}
\value{
A list with \code{u}, \code{n}, \code{shift}, \code{first_difference}, \code{value_at_origin}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
StepSeq(V)
}
