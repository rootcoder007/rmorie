% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ot_quantiles}
\alias{.ot_quantiles}
\title{.ot_quantiles}
\usage{
.ot_quantiles(x, grid)
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{grid}{Iterated over elementwise, with \code{vapply}.}
}
\description{
A step of the helpers_ot implementation. Called by \code{Otsd}. See the file header for the source the module follows.
}
\value{
A vector, from \code{vapply}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.ot_quantiles(V, V)
}
