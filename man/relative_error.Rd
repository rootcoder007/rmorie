% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{relative_error}
\alias{relative_error}
\title{relative_error}
\usage{
relative_error(X, M)
}
\arguments{
\item{X}{A vector; indexed elementwise.}
\item{M}{Passed to \code{.meglt_mat}.}
}
\description{
A step of the meglt_native implementation. Called by \code{morie_meglt}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
relative_error(V, V)
}
