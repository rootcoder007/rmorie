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
\item{v}{See Usage.}
\item{eps}{Defaults to \code{.vitlneps}.}
}
\description{
Part of the helpers_vit implementation; see the file header for the source it follows.
}
\value{
A numeric value.
}
\keyword{internal}
