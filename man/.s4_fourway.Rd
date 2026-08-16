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
\item{theta}{A vector; indexed elementwise.}
\item{beta}{A vector; indexed elementwise.}
\item{cbar}{A vector; its length is taken.}
\item{a}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
\item{astar}{Numeric; combined arithmetically in the body. Defaults to \code{0}.}
\item{m}{Numeric; combined arithmetically in the body. Defaults to \code{0}.}
}
\description{
A step of the s04core implementation. Called by \code{Causmnde}, \code{Intmd4}, \code{Intvse} and 2 others in the module. See the file header for the source the module follows.
}
\value{
A list with \code{cde}, \code{intref}, \code{intmed}, \code{pie}, \code{te}.
}
\keyword{internal}
\examples{
.s4_fourway(theta = 0.5, beta = 0.5, cbar = c(1, 2, 3, 4, 5, 6, 7, 8))
}
