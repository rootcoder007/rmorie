% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.fz_seq}
\alias{.fz_seq}
\title{.fz_seq}
\usage{
.fz_seq(from, to, n)
}
\arguments{
\item{from}{Passed to \code{seq}.}
\item{to}{Passed to \code{seq}.}
\item{n}{Passed to \code{seq}.}
}
\description{
A step of the fauzi_native implementation. Called by \code{morie_fauzi_b2_coefficient}, \code{morie_fauzi_boundary_free_kde}, \code{morie_fauzi_cumulative_survival_1} and 7 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{seq}.
}
\keyword{internal}
\examples{
.fz_seq(from = 5L, to = 5L, n = 5L)
}
