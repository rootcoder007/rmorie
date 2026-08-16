% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tf_spline}
\alias{.tf_spline}
\title{.tf_spline}
\usage{
.tf_spline(xs, ys, xq)
}
\arguments{
\item{xs}{A vector; its length is taken and its elements indexed.}
\item{ys}{A vector; indexed elementwise.}
\item{xq}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the rangayyan_tf implementation. Called by \code{.tf_sift}, \code{Imf}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
.tf_spline(xs = c(1, 2, 3, 4, 5, 6, 7, 8), ys = c(1, 2, 3, 4, 5, 6, 7, 8), xq = c(1, 2, 3, 4, 5, 6, 7, 8))
}
