% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.rfkprimes}
\alias{.rfkprimes}
\title{The first k primes, for the Halton bases}
\usage{
.rfkprimes(k)
}
\arguments{
\item{k}{See Usage.}
}
\description{
A step of the rfkrn implementation. Called by \code{Rfkrn}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
.rfkprimes(k = 5L)
}
