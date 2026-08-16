% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{mean_scale}
\alias{mean_scale}
\title{mean_scale}
\usage{
mean_scale(x, context = NULL)
}
\arguments{
\item{x}{Passed to \code{.chronos_vec}.}
\item{context}{Defaults to \code{NULL}.}
}
\description{
A step of the chronos_native implementation. Called by \code{tokenize}. See the file header for the source the module follows.
}
\value{
A list with \code{scaled}, \code{scale}, \code{degenerate}, \code{context}, \code{preserves_zero}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
mean_scale(V)
}
