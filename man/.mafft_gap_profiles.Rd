% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mafft_gap_profiles}
\alias{.mafft_gap_profiles}
\title{.mafft_gap_profiles}
\usage{
.mafft_gap_profiles(group, weights)
}
\arguments{
\item{group}{A vector; its length is taken and its elements indexed.}
\item{weights}{A vector; indexed elementwise.}
}
\description{
A step of the mafft_native implementation. Called by \code{.mafft_nw}. See the file header for the source the module follows.
}
\value{
A list with \code{gs}, \code{ge}.
}
\keyword{internal}
