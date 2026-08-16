% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.gpmat}
\alias{.gpmat}
\title{.gpmat}
\usage{
.gpmat(A)
}
\arguments{
\item{A}{A matrix; passed to \code{nrow}.}
}
\description{
A step of the helpers_gp_core implementation. Called by \code{.gpannsse}, \code{.gpblueblup}, \code{.gpbrier} and 18 others in the module. See the file header for the source the module follows.
}
\value{
A matrix, from \code{matrix}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.gpmat(V)
}
