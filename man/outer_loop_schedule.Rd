% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{outer_loop_schedule}
\alias{outer_loop_schedule}
\title{outer_loop_schedule}
\usage{
outer_loop_schedule(alpha, C, examine_all)
}
\arguments{
\item{alpha}{See Usage.}
\item{C}{See Usage.}
\item{examine_all}{See Usage.}
}
\description{
Part of the smoopt_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{indices}, \code{kind}, \code{n_non_bound}, \code{note}.
}
\keyword{internal}
\examples{
outer_loop_schedule(alpha = 0.5, C = c(1, 2, 3, 4, 5, 6, 7, 8), examine_all = c(1, 2, 3, 4, 5, 6, 7, 8))
}
