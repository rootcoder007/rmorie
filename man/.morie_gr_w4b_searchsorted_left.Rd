% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_gr_w4b_searchsorted_left}
\alias{.morie_gr_w4b_searchsorted_left}
\title{.morie_gr_w4b_searchsorted_left}
\usage{
.morie_gr_w4b_searchsorted_left(e, x)
}
\arguments{
\item{e}{A vector; its length is taken.}
\item{x}{A vector; its length is taken.}
}
\description{
A step of the geron_w4b_native implementation. Called by \code{morie_geron_histogram_gradient_boosting}. See the file header for the source the module follows.
}
\value{
A vector, from \code{vapply}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_gr_w4b_searchsorted_left(V, V)
}
