% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ot_sortbasis}
\alias{.ot_sortbasis}
\title{.ot_sortbasis}
\usage{
.ot_sortbasis(edges)
}
\arguments{
\item{edges}{A vector; its length is taken and its elements indexed.}
}
\description{
A step of the helpers_ot implementation. Called by \code{.ot_complete_tree}, \code{.ot_emd}. See the file header for the source the module follows.
}
\value{
The value of \code{[}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.ot_sortbasis(V)
}
