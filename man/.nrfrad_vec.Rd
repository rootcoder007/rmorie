% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.nrfrad_vec}
\alias{.nrfrad_vec}
\title{S3 helper: ensure a numeric vector (k.vec)}
\usage{
.nrfrad_vec(p)
}
\arguments{
\item{p}{Numeric; the body checks with \code{is.numeric}.}
}
\description{
A step of the nrfrad_native implementation. Called by \code{density_is_view_independent}, \code{positional_encoding}, \code{ray_points} and 2 others in the module. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
