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
\item{x}{See Usage.}
\item{weights}{See Usage.}
\item{w0}{Defaults to \code{NULL}.}
}
\description{
surfaces between classes are hyperplanes, so a linear machine carves the space into CONVEX regions -- which is why it cannot separate classes whose regions are not convex, however many features are added.
}
\value{
A list with \code{d}, \code{assigned}, \code{margin}, \code{n_classes}, \code{regions_are_convex}, \code{decision_surfaces_are_hyperplanes}, \code{method}.
}
\keyword{internal}
