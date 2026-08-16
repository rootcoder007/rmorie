% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.s4_quantile7}
\alias{.s4_quantile7}
\title{# R quantile type 7, spelled out so the two arms cannot drift}
\usage{
.s4_quantile7(x, p)
}
\arguments{
\item{x}{A vector; its length is taken and its elements indexed.}
\item{p}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the s04core implementation. Called by \code{Nprphet}, \code{Winz}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.s4_quantile7(V, V)
}
