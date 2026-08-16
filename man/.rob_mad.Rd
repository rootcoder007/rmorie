% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.rob_mad}
\alias{.rob_mad}
\title{.rob_mad}
\usage{
.rob_mad(r)
}
\arguments{
\item{r}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the robust_native implementation. Called by \code{.rob_s_scale}, \code{morie_rob_huber}, \code{morie_rob_m}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.rob_mad(V)
}
