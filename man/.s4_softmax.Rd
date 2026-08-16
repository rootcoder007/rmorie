% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.s4_softmax}
\alias{.s4_softmax}
\title{.s4_softmax}
\usage{
.s4_softmax(v)
}
\arguments{
\item{v}{Numeric; passed to \code{max}.}
}
\description{
A step of the s04core implementation. Called by \code{Gat}, \code{Gpmoe}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.s4_softmax(V)
}
