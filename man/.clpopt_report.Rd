% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.clpopt_report}
\alias{.clpopt_report}
\title{Internal: report a successful solution. Mirrors clpopt._report}
\usage{
.clpopt_report(T, basis, cv, n, m, total, rule)
}
\arguments{
\item{T}{See Usage.}
\item{basis}{See Usage.}
\item{cv}{See Usage.}
\item{n}{See Usage.}
\item{m}{See Usage.}
\item{total}{See Usage.}
\item{rule}{See Usage.}
}
\description{
Part of the clpopt_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{status}, \code{x}, \code{fun}, \code{duals}, \code{basis}, \code{reduced_costs}, \code{degenerate}, \code{multiple_optima}, \code{alternate_entering}, \code{rule}, \code{method}.
}
\keyword{internal}
