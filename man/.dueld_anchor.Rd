% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.dueld_anchor}
\alias{.dueld_anchor}
\title{.dueld_anchor}
\usage{
.dueld_anchor(a, mode)
}
\arguments{
\item{a}{A vector; its length is taken and its elements indexed.}
\item{mode}{One of \code{"max"}, \code{"mean"}.}
}
\description{
A step of the dueldqn_native implementation. Called by \code{dueling_aggregate}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
.dueld_anchor(a = c(1, 2, 3, 4, 5, 6, 7, 8), mode = 5L)
}
