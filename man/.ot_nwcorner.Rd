% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ot_nwcorner}
\alias{.ot_nwcorner}
\title{.ot_nwcorner}
\usage{
.ot_nwcorner(a, b)
}
\arguments{
\item{a}{A vector; its length is taken.}
\item{b}{A vector; its length is taken.}
}
\description{
A step of the helpers_ot implementation. Called by \code{.ot_emd}. See the file header for the source the module follows.
}
\value{
A list with \code{T}, \code{basis}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.ot_nwcorner(V, V)
}
