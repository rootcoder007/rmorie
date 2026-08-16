% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.s03logsumexp}
\alias{.s03logsumexp}
\title{.s03logsumexp}
\usage{
.s03logsumexp(v)
}
\arguments{
\item{v}{A vector; its length is taken.}
}
\description{
A step of the helpers_s03 implementation. Called by \code{Dpgmm}, \code{Hdpgmm}, \code{Hdplda} and 4 others in the module. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.s03logsumexp(V)
}
