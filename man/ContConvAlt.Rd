% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{ContConvAlt}
\alias{ContConvAlt}
\title{Eq (3.31): y(t) = integral h(tau) x(t - tau) d tau, given by the book}
\usage{
ContConvAlt(x, h, dt = 1, t = NULL) 
}
\arguments{
\item{x}{Passed to \code{ContConv}.}
\item{h}{Passed to \code{ContConv}.}
\item{dt}{Passed to \code{ContConv}. Defaults to \code{1}.}
\item{t}{Passed to \code{ContConv}.}
}
\description{
as an equivalent result.  Computed the other way round and compared against eq (3.30) rather than asserted equivalent.
}
\value{
The value of \code{swapped}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
ContConvAlt(V, V)
}
