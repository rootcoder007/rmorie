% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_dp_rlaplace}
\alias{.morie_dp_rlaplace}
\title{Laplace(0, b) by inverse transform; R has no rlaplace in base}
\usage{
.morie_dp_rlaplace(n, scale)
}
\arguments{
\item{n}{See Usage.}
\item{scale}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the dp_native implementation. Called by \code{morie_dp_count}, \code{morie_dp_histogram}, \code{morie_dp_kmeans} and 3 others in the module. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
