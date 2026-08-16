% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.if_wvar}
\alias{.if_wvar}
\title{.if_wvar}
\usage{
.if_wvar(v, w)
}
\arguments{
\item{v}{Numeric; combined arithmetically in the body.}
\item{w}{Numeric; passed to \code{sum}.}
}
\description{
A step of the infcrv implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.if_wvar(V, V)
}
