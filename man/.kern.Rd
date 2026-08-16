% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.kern}
\alias{.kern}
\title{.kern}
\usage{
.kern(u)
}
\arguments{
\item{u}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the tmlcps_native implementation. Called by \code{.smooth_at}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.kern(V)
}
