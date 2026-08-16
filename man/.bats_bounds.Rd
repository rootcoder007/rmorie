% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.bats_bounds}
\alias{.bats_bounds}
\title{.bats_bounds}
\usage{
.bats_bounds(spec)
}
\arguments{
\item{spec}{A list; the body reads \code{$damped}, \code{$harmonics}, \code{$p}, \code{$periods}, \code{$q}, \code{$use_box_cox}, \code{$use_trend} from it.}
}
\description{
A step of the bats_native implementation. Called by \code{.fit_spec}. See the file header for the source the module follows.
}
\value{
A list with \code{lo}, \code{hi}.
}
\keyword{internal}
