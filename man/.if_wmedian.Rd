% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.if_wmedian}
\alias{.if_wmedian}
\title{Lower weighted median: the smallest value whose cumulative weight}
\usage{
.if_wmedian(v, w)
}
\arguments{
\item{v}{A vector; indexed elementwise.}
\item{w}{A vector; indexed elementwise.}
}
\description{
reaches half the total.
}
\value{
The value of \code{[}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.if_wmedian(V, V)
}
