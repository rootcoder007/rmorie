% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{LsiSer}
\alias{LsiSer}
\title{Eqs (3.43)-(3.45): s = x*h1, y = s*h2 = x*h, h = h1*h2.  One method,}
\usage{
LsiSer(x, h1, h2)
}
\arguments{
\item{x}{See Usage.}
\item{h1}{See Usage.}
\item{h2}{See Usage.}
}
\description{
so one function; the equivalence in eq (3.44) is measured, not asserted -- filtering twice is compared against filtering once with h.
}
\value{
A list with \code{s}, \code{y}, \code{h}, \code{y_via_combined}, \code{max_difference}, \code{equivalent}, \code{method}.
}
\keyword{internal}
\examples{
LsiSer(x = c(1, 2, 3, 4, 5, 6, 7, 8), h1 = c(1, 2, 3, 4, 5, 6, 7, 8), h2 = c(1, 2, 3, 4, 5, 6, 7, 8))
}
