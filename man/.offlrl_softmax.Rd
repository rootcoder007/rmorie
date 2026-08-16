% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.offlrl_softmax}
\alias{.offlrl_softmax}
\title{.offlrl_softmax}
\usage{
.offlrl_softmax(v)
}
\arguments{
\item{v}{Numeric; passed to \code{max}.}
}
\description{
A step of the offlrl_native implementation. Called by \code{offlrl}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.offlrl_softmax(V)
}
