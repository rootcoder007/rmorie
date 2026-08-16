% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.fz_check_sample}
\alias{.fz_check_sample}
\title{.fz_check_sample}
\usage{
.fz_check_sample(x, min_n = 2L)
}
\arguments{
\item{x}{A vector; its length is taken.}
\item{min_n}{Defaults to \code{2L}.}
}
\description{
A step of the fauzi_native implementation. Called by \code{.fz_cs_common}, \code{morie_fauzi_boundary_free_kde}, \code{morie_fauzi_conditions_c1_c6} and 7 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{x}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.fz_check_sample(V)
}
