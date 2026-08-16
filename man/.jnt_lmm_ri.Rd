% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.jnt_lmm_ri}
\alias{.jnt_lmm_ri}
\title{.jnt_lmm_ri}
\usage{
.jnt_lmm_ri(y, X, grp, ord, iters = 200L)
}
\description{
.jnt_lmm_ri
}
\value{
A list with \code{beta}, \code{t2}, \code{s2}, \code{b}.
}
\keyword{internal}
\examples{
.jnt_lmm_ri(y = 5L, X = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2), grp = data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9)), ord = c(1, 2, 3, 4, 5, 6, 7, 8))
}
