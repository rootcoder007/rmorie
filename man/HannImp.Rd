% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{HannImp}
\alias{HannImp}
\title{Eq (3.101): three nonzero taps and nothing else.  The response is}
\usage{
HannImp(n = NULL)
}
\arguments{
\item{n}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
}
\description{
FINITE, which is what "FIR" names, and it sums to 1, so a constant passes through unchanged.
}
\value{
A list with \code{h}, \code{value}, \code{index}, \code{n_taps}, \code{sum}, \code{finite}, \code{symmetric}, \code{method}.
}
\keyword{internal}
\examples{
HannImp()
}
