% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.s03allocCheck}
\alias{.s03allocCheck}
\title{Shared by Neyman, Neymal and Propal.  Coerce and validate the common}
\usage{
.s03allocCheck(N_h, S_h, n)
}
\arguments{
\item{N_h}{Passed to \code{.s03vec}.}
\item{S_h}{Optional; may be \code{NULL}. Passed to \code{.s03vec}.}
\item{n}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
stratum arguments; S is all ones when S_h is NULL, which turns Neyman allocation into proportional allocation.
}
\value{
A list with \code{M}, \code{S}, \code{m0}, \code{H}.
}
\keyword{internal}
\examples{
.s03allocCheck(N_h = c(1, 2, 3, 4, 5, 6, 7, 8), S_h = c(1, 2, 3, 4, 5, 6, 7, 8), n = 5L)
}
