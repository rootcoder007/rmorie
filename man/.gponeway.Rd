% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.gponeway}
\alias{.gponeway}
\title{Chapter 1: balanced one-way layout, eqs (1.2)-(1.5)}
\usage{
.gponeway(groups)
}
\arguments{
\item{groups}{Iterated over elementwise, with \code{lapply}.}
}
\description{
------------------- Part of the helpers_gp_core implementation; see the file header for the source it follows.
}
\value{
A list with \code{grand_mean}, \code{sd_single_mean}, \code{group_means}, \code{sd_residual}, \code{deviations}, \code{sigma2_b}, \code{icc}, \code{ms_between}, \code{ms_within}.
}
\keyword{internal}
\examples{
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
.gponeway(D)
}
