% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_kamath_multihead_concat}
\alias{morie_kamath_multihead_concat}
\title{morie_kamath_multihead_concat}
\usage{
morie_kamath_multihead_concat(heads, W_O)
}
\arguments{
\item{heads}{List of head outputs.}
\item{W_O}{Output projection.}
}
\description{
morie_kamath_multihead_concat
}
\value{
A list with \code{output}, \code{heads}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_kamath_multihead_concat(V, V)
}
