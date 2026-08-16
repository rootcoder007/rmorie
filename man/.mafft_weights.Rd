% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mafft_weights}
\alias{.mafft_weights}
\title{.mafft_weights}
\usage{
.mafft_weights(k)
}
\arguments{
\item{k}{A count; the body uses it as \code{rep(...)}.}
}
\description{
A step of the mafft_native implementation. Called by \code{iterative_refine}, \code{progressive_align}, \code{wsp_score}. See the file header for the source the module follows.
}
\value{
The value of \code{rep}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.mafft_weights(V)
}
