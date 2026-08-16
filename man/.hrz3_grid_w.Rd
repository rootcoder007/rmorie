% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.hrz3_grid_w}
\alias{.hrz3_grid_w}
\title{Equispaced grid on [0, 1] with trapezoid quadrature weights}
\usage{
.hrz3_grid_w(m)
}
\arguments{
\item{m}{A count; the body uses it as \code{seq_len(...)}.}
}
\description{
A step of the helpers_hrz3 implementation. Called by \code{Hrznpiv}, \code{Hrztiku}. See the file header for the source the module follows.
}
\value{
A list with \code{z}, \code{w}.
}
\keyword{internal}
