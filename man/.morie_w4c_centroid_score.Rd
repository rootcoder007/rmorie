% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_w4c_centroid_score}
\alias{.morie_w4c_centroid_score}
\title{.morie_w4c_centroid_score}
\usage{
.morie_w4c_centroid_score(Xb, yb)
}
\arguments{
\item{Xb}{A matrix; indexed by row and column.}
\item{yb}{See Usage.}
}
\description{
A step of the geron_w4c_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{function}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
.morie_w4c_centroid_score(D, V)
}
