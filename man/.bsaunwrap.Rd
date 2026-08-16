% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.bsaunwrap}
\alias{.bsaunwrap}
\title{.bsaunwrap}
\usage{
.bsaunwrap(ph)
}
\arguments{
\item{ph}{A vector; its length is taken and its elements indexed.}
}
\description{
A step of the rangayyan_phys implementation. Called by \code{CLogProd}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.bsaunwrap(V)
}
