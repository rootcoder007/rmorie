% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{length_weight}
\alias{length_weight}
\title{length_weight}
\usage{
length_weight(length, l_min = 2500, l_ref = 1e+05)
}
\arguments{
\item{length}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{l_min}{Numeric; combined arithmetically in the body. Defaults to \code{2500}.}
\item{l_ref}{Numeric; combined arithmetically in the body. Defaults to \code{1e+05}.}
}
\description{
A step of the metabd_native implementation. Called by \code{composite_distance}, \code{morie_metabd}. See the file header for the source the module follows.
}
\value{
A list with \code{weight}, \code{length}, \code{below_minimum}.
}
\keyword{internal}
\examples{
length_weight(length = 5L)
}
