% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Ustep}
\alias{Ustep}
\title{Eq (3.27): u(t) = 1 for t > 0, 0 otherwise.  Strict: u(0) = 0 here,}
\usage{
Ustep(t, shift = 0)
}
\arguments{
\item{t}{See Usage.}
\item{shift}{Defaults to \code{0}.}
}
\description{
whereas the discrete step of eq (3.35) has u(0) = 1.  They are separate definitions, not one sampled from the other.
}
\value{
A list with \code{u}, \code{t}, \code{shift}, \code{value_at_origin}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Ustep(V)
}
