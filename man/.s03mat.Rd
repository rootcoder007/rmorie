% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.s03mat}
\alias{.s03mat}
\title{.s03mat}
\usage{
.s03mat(x)
}
\arguments{
\item{x}{Optional; may be \code{NULL}. A matrix; the body checks with \code{is.matrix}.}
}
\description{
A step of the helpers_s03 implementation. Called by \code{.bkw_influence}, \code{.cfa_cov}, \code{.ch_ols_se} and 236 others in the module. See the file header for the source the module follows.
}
\value{
A matrix, from \code{matrix}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.s03mat(V)
}
