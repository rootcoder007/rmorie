% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.bt_median}
\alias{.bt_median}
\title{.bt_median}
\usage{
.bt_median(sorted_vals)
}
\arguments{
\item{sorted_vals}{A vector; its length is taken and its elements indexed.}
}
\description{
A step of the btcimed implementation. Called by \code{Btcimed}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.bt_median(V)
}
