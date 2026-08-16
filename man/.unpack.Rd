% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.unpack}
\alias{.unpack}
\title{.unpack}
\usage{
.unpack(spec, theta)
}
\arguments{
\item{spec}{A list; the body reads \code{$damped}, \code{$harmonics}, \code{$p}, \code{$periods}, \code{$q}, \code{$use_box_cox}, \code{$use_trend} from it.}
\item{theta}{A vector; indexed elementwise.}
}
\description{
A step of the bats_native implementation. Called by \code{.forecast}, \code{.sarima_fit}, \code{bats_filter} and 1 others in the module. See the file header for the source the module follows.
}
\value{
A list with \code{alpha}, \code{beta}, \code{phi}, \code{gam}, \code{ar}, \code{ma}, \code{omega}.
}
\keyword{internal}
