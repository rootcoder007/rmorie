% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.jnt_cox}
\alias{.jnt_cox}
\title{.jnt_cox}
\usage{
.jnt_cox(t, d, Z, iters = 50L)
}
\description{
.jnt_cox
}
\value{
A list with \code{beta}, \code{se}, \code{ll}.
}
\keyword{internal}
\examples{
.jnt_cox(t = 5L, d = 5L, Z = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2))
}
