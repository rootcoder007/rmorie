% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mafft_get}
\alias{.mafft_get}
\title{.mafft_get}
\usage{
.mafft_get(M, a, b)
}
\arguments{
\item{M}{A vector; indexed elementwise.}
\item{a}{Passed to \code{paste}.}
\item{b}{Passed to \code{paste}.}
}
\description{
A step of the mafft_native implementation. Called by \code{.mafft_site_score}, \code{normalized_similarity_matrix}, \code{wsp_score}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
