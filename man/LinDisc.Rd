% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{LinDisc}
\alias{LinDisc}
\title{Section 10.4.1: d_i(x) = w_i^T x + w_i0, assign to the largest.  The}
\usage{
LinDisc(x, weights, w0 = NULL)
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{weights}{A matrix; passed to \code{as.matrix}.}
\item{w0}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
surfaces between classes are hyperplanes, so a linear machine carves the space into CONVEX regions -- which is why it cannot separate classes whose regions are not convex, however many features are added.
}
\value{
A list with \code{d}, \code{assigned}, \code{margin}, \code{n_classes}, \code{regions_are_convex}, \code{decision_surfaces_are_hyperplanes}, \code{method}.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
S <- c("a", "b", "c")
LinDisc(S, M)
}
