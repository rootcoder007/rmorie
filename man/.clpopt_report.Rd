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
\item{T}{A matrix; indexed by row and column.}
\item{basis}{A vector; indexed elementwise.}
\item{cv}{A vector; indexed elementwise.}
\item{n}{A count; the body uses it as \code{seq_len(...)}.}
\item{m}{A count; the body uses it as \code{seq_len(...)}.}
\item{total}{Accepted by the signature and not used anywhere in the body.}
\item{rule}{Compared against \code{"bland"}.}
}
\description{
A step of the clpopt_native implementation. Called by \code{simplex}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{status}, \code{x}, \code{fun}, \code{duals}, \code{basis}, \code{reduced_costs}, \code{degenerate}, \code{multiple_optima}, \code{alternate_entering}, \code{rule}, \code{method}.
}
\keyword{internal}
