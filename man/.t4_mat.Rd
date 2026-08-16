% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.t4_mat}
\alias{.t4_mat}
\title{.t4_mat}
\usage{
.t4_mat(X)
}
\arguments{
\item{X}{A matrix; passed to \code{nrow}.}
}
\description{
A step of the tail4_core implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A matrix, from \code{matrix}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.t4_mat(V)
}
