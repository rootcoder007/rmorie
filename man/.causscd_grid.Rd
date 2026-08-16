% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.causscd_grid}
\alias{.causscd_grid}
\title{.causscd_grid}
\usage{
.causscd_grid(Y, treated, t_post)
}
\arguments{
\item{Y}{See Usage.}
\item{treated}{See Usage.}
\item{t_post}{See Usage.}
}
\description{
A step of the causscd_native implementation. Called by \code{sdid}, \code{time_weights}, \code{unit_weights}. See the file header for the source the module follows.
}
\value{
A list with \code{rows}, \code{n}, \code{T}, \code{tr}, \code{t_post}.
}
\keyword{internal}
