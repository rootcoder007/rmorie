% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mor_cp_segmeans}
\alias{.mor_cp_segmeans}
\title{Segment means of the 0-based bounds vector}
\usage{
.mor_cp_segmeans(x, bounds)
}
\arguments{
\item{x}{A vector; indexed elementwise.}
\item{bounds}{A vector; its length is taken and its elements indexed.}
}
\description{
A step of the pelt_native implementation. Called by \code{morie_binseg}, \code{morie_pelt}. See the file header for the source the module follows.
}
\value{
A vector, from \code{vapply}.
}
\keyword{internal}
