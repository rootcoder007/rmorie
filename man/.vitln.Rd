% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.vitln}
\alias{.vitln}
\title{LN(v) = (v - mean v) / sqrt(pop.var v + eps), gamma = 1, beta = 0}
\usage{
.vitln(v, eps = .vitlneps)
}
\arguments{
\item{v}{A vector; its length is taken.}
\item{eps}{Numeric; combined arithmetically in the body. Defaults to \code{.vitlneps}.}
}
\description{
A step of the helpers_vit implementation. Called by \code{.vitlnrows}, \code{Vitfwd}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.vitln(V)
}
