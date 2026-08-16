% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.s4_fourway}
\alias{.s4_fourway}
\title{# VanderWeele four-way decomposition from fitted coefficients}
\usage{
.s4_fourway(theta, beta, cbar, a = 1, astar = 0, m = 0)
}
\arguments{
\item{theta}{See Usage.}
\item{beta}{See Usage.}
\item{cbar}{See Usage.}
\item{a}{Defaults to \code{1}.}
\item{astar}{Defaults to \code{0}.}
\item{m}{Defaults to \code{0}.}
}
\description{
Part of the s04core implementation; see the file header for the source it follows.
}
\value{
A list with \code{cde}, \code{intref}, \code{intmed}, \code{pie}, \code{te}.
}
\keyword{internal}
