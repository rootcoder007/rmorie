% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_sb_pairs}
\alias{.morie_sb_pairs}
\title{.morie_sb_pairs}
\usage{
.morie_sb_pairs(coords, z)
}
\arguments{
\item{coords}{A matrix; passed to \code{as.matrix}.}
\item{z}{See Usage.}
}
\description{
A step of the schaben_native implementation. Called by \code{morie_cressie_hawkins}, \code{morie_matheron_estimator}, \code{morie_variogram_composite_likelihood}. See the file header for the source the module follows.
}
\value{
A list with \code{h}, \code{d}.
}
\keyword{internal}
