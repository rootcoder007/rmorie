% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.gpinv}
\alias{.gpinv}
\title{.gpinv}
\usage{
.gpinv(A)
}
\arguments{
\item{A}{A matrix; passed to \code{solve}.}
}
\description{
A step of the helpers_gp_core implementation. Called by \code{.gpblueblup}, \code{.gplmmloglik}, \code{.gpolsfit} and 1 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{.gppinv}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.gpinv(V)
}
