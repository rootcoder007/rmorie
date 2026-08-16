% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mvsinv}
\alias{.mvsinv}
\title{Inverse of a symmetric positive definite matrix, by columns,}
\usage{
.mvsinv(A, ridge = 1e-12)
}
\arguments{
\item{A}{See Usage.}
\item{ridge}{Defaults to \code{1e-12}.}
}
\description{
symmetrised so that the two arms cannot drift on round-off.
}
\value{
A numeric value.
}
\keyword{internal}
