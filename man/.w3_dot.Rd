% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.w3_dot}
\alias{.w3_dot}
\title{Compensated dot product. Not sum(a * b), same reason}
\usage{
.w3_dot(a, b)
}
\arguments{
\item{a}{A vector; its length is taken and its elements indexed.}
\item{b}{A vector; indexed elementwise.}
}
\description{
A step of the helpers_w3num implementation. Called by \code{.baysmplr_build_tree}, \code{.chemsc_angle}, \code{.hyper2_elliptical} and 21 others in the module. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
